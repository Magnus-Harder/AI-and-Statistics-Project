load("armdataCleaned.RData")



armdata[[1]][[1]][[1]]

data_dist <- matrix(nrow=1600,ncol = 300)

for (dimension in 1:300){
  idx <- 1 
  for (e in 1:16){
    for (p in 1:10){
      for (r in 1:10){
        data_dist[idx,dimension]<- c(armdata[[e]][[p]][[r]])[dimension] 
        idx <- idx + 1
      }
    }
  }
}


norm <- function(x){
  
  return(sqrt(sum(x^2)))
}
normtest <- function(x){
  # What test to use
  test <- ks.test(x,"pnorm",mean(x),sd(x))
  return(test$p.value)
}


apply(armdata[[1]][[1]][[1]],1,norm)



norm_data <- matrix(nrow=1600,ncol =99)

for (dimension in 1:99){
  idx <- 1 
  for (e in 1:16){
    for (p in 1:10){
      for (r in 1:10){
        if (armdata[[e]][[p]][[r]][dimension,3] < armdata[[e]][[p]][[r]][dimension+1,3]){
          x <- armdata[[e]][[p]][[r]][dimension,]-armdata[[e]][[p]][[r]][dimension+1,]
          norm_data[idx,dimension]<- -norm(armdata[[e]][[p]][[r]][dimension,]-armdata[[e]][[p]][[r]][dimension+1,])
        }
        else {
          x <- armdata[[e]][[p]][[r]][dimension,]-armdata[[e]][[p]][[r]][dimension+1,]
          norm_data[idx,dimension] <- norm(x)
        }
        idx <- idx + 1
      }
    }
  }
}

shapiro.test(norm_data[,1])

qqnorm(norm_data[,1])
qqline(norm_data[,1])


p_vals <- apply(norm_data,2,normtest)
sum(p_vals >0.05)

p_vals_adj <- p.adjust(p_vals, method = "BH")


sum(p_vals_adj > 0.05)

X <- c(1:100)
p_val <- sort((p_vals_adj[1:100]))
plot(X,y=p_val,main="Adjusted P-values Kolmogorov-smirnov test normality",col="blue",ylim = c(0,1))
lines(X,rep(0.05,100),col="black",type="l",lty=2)
legend("bottomright",legend=c("p-values_x","p-values_y","p-values_z","alpha=0.05"),col=c("blue","red","green","black"),lty=c(1,1,1,2))

















  
data_max_pos <- matrix(nrow=1600,ncol=3)
idx <- 1
for (e in 1:16){
  for (p in 1:10){
    for (r in 1:10){
      max_idx <-  which.max(armdata[[e]][[p]][[r]][,3])
      data_max_pos[idx,1] <- armdata[[e]][[p]][[r]][max_idx,1]
      data_max_pos[idx,2] <- armdata[[e]][[p]][[r]][max_idx,2]
      data_max_pos[idx,3] <- max(armdata[[e]][[p]][[r]][,3])
      idx <- idx + 1
    }
  }
}

data_max_pos

normtest(data_max_pos[,1])
normtest(data_max_pos[,2])
normtest(data_max_pos[,3])

person <- as.factor(rep(rep(1:10,each=10),16))
experiment <- as.factor(rep(1:16,each=100))
test <- manova(lm(data_max_pos[1:300,]~person[1:300]+experiment[1:300])) # Experiment is same as checking for effect of obstacle heigth

summary(test)
normtest(test$residuals)
plot(test$residuals)
hist(test$residuals[,1])
hist(test$residuals[,2])
hist(test$residuals[,3])

qqnorm(test$residuals[,1])
qqline(test$residuals[,1])

qqnorm(test$residuals[,2])
qqline(test$residuals[,2])


# Method: Kruskal wallis mean for repetition for each person pr. experiemtns
idx_start <- 1
idx_slut <- 10 
data_mean_rep <- matrix(nrow=160,ncol=300)
for (i in 1:160){
  data_mean_rep[i,] <- apply(data_dist[idx_start:idx_slut,],2,mean)
  idx_start <- idx_start + 10
  idx_slut <- idx_slut + 10
}

normtest <- function(x){
  # What test to use
  test <- ks.test(x,"pnorm",mean(x),sd(x))
  return(test$p.value)
}

p_vals_x <- apply(data_mean_rep[,1:100],2,normtest)
p_vals_x
sum(p_vals_x >0.05)


p_vals_y <- apply(data_mean_rep[,101:200],2,normtest)
p_vals_y
sum(p_vals_y >0.05)


p_vals_z <- apply(data_mean_rep[,201:300],2,normtest)
p_vals_z
sum(p_vals_z >0.05)


p_vals_adj <- p.adjust(c(p_vals_x,p_vals_y,p_vals_z), method = "BH")


sum(p_vals_adj[1:100] >0.05)
sum(p_vals_adj[101:200] >0.05)
sum(p_vals_adj[201:300] >0.05)

X <- c(1:100)
p_val <- sort((p_vals_adj[1:100]))
plot(X,y=p_val,main="Adjusted P-values Kolmogorov-smirnov test normality",col="blue",ylim = c(0,1))
lines(X,sort(p_vals_adj[101:200]),col="red",type="p")
lines(X,sort(p_vals_adj[201:300]),col="green",type="p")
lines(X,rep(0.05,100),col="black",type="l",lty=2)
legend("bottomright",legend=c("p-values_x","p-values_y","p-values_z","alpha=0.05"),col=c("blue","red","green","black"),lty=c(1,1,1,2))

data_dist_stan <- matrix(NA,nrow=160,ncol=300)
sds <- apply(data_mean_rep,2,sd)
for (idx in 1:300) {
  data_dist_stan[,idx]  <-(data_mean_rep[,idx]-mean(data_mean_rep[,idx]))/sd(data_mean_rep[,idx])
  
}


person <- as.factor(rep(c(1:10),16))
experiment <- as.factor(rep(c(1:16),each=10))


p_effect_per <- rep(NA,300)
p_effect_exp <- rep(NA,300)
for (i in 1:300){
  g <- anova(lm(data_mean_rep[,i]~person+experiment))
  p_effect_per[i] <- g$`Pr(>F)`[1]
  p_effect_exp[i] <- g$`Pr(>F)`[2]
  
}

p_effect_per_adj <- p.adjust(p_effect_per,method = "BH")
p_effect_exp_adj <- p.adjust(p_effect_exp,method = "BH")

par(mfrow=c(1,1))
plot(p_effect_exp_adj)
plot(p_effect_per_adj)


summary(manova(data_mean_rep[,1:100]~person+experiment))
summary(manova(data_dist_stan[,100]~person+experiment))

summary(manova(data_mean_rep[,101:200]~person+experiment))
summary(manova(data_dist_stan[,101:200]~person+experiment))

summary(manova(data_mean_rep[,201:300]~person+experiment))
summary(manova(data_dist_stan[,201:300]~person+experiment))


# Method: Kruskal wallis test for each dimension comparing one-way with person
# Not able since this requires independent data, and not repeated measure
  p_vals <- rep(NA,300)
  for (i in 1:300){
    p_vals[i] <- kruskal.test(c(data_dist[,i])~person)$p.value
    
  }
  p_adj <- p.adjust(p_vals,method="BH")
  mean(p_adj)
  plot(sort(p_adj))

# Method: Manova
# Data has uniform variance 
# Can we assume colinearity and normality for the data?
data_dist_stan <- matrix(NA,nrow=1600,ncol=300)
sds <- apply(data_dist,2,sd)
for (idx in 1:300) {
  data_dist_stan[,idx]  <-(data_dist[,idx]-mean(data_dist[,idx]))/sd(data_dist[,idx])
  
}
summary(manova(data_dist_stan~person*experiment))





kruskal.test(c(data_dist[,1])~person)

kruskal.test(c(data_dist[,1])~person_experiment)


summary(manova(data_dist~Repetetion))

summary(manova(data_dist~person))
summary(manova(data_dist~experiment))
summary(manova(data_dist~person*experiment))



kruskal.test(c(data_dist)~person)
kruskal.test(c(data_dist)~experiment)

length(experiment)



idx_start <- 1
idx_slut <- 10
test <- rep(NA,160)
for (i in 1:160) {
  test[i] <- mean(data_dist[idx_start:idx_slut,1])
  idx_slut <- idx_slut + 10 
  idx_start <- idx_start + 10
}

anova(lm(test~person1))

mean(test[c(1,17,)])
idx <- 290
x1_stand <-(data_dist[,idx]-mean(data_dist[,idx]))/sd(data_dist[,idx])
mean(x1_stand)
sd(x1_stand)


multkw()
