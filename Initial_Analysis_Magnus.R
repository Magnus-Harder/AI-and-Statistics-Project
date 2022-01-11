library(reticulate)

py_config()

# Sun Jan  9 18:33:58 2022 ------------------------------
load("armdata.RData")

#' How to Call the Data (Using list reqiures dubble [[]]  indexing)
#' First layer is Experiment, there is a total of 16 different experiemnts in first layer
#' Second layer is Persons, There is 10 different persons 
#' Third layer is repetetions, there is 10 repetetions for each person in each experiemtn
#' Totalt number of experiments is thus 16 · 10 · 10 = 1600
#' Callling Experiment 4, Person 3, Repetetion 8, would thus be armdata  

# Missing values
armdata[[5]][[9]][[1]]  # missing first observation
armdata[[5]][[9]][[1]][1,] <-  armdata[[5]][[9]][[1]][2,]


armdata[[7]][[9]][[2]] # Missing first observation
armdata[[7]][[9]][[2]][1,] <- armdata[[7]][[9]][[2]][2,] 


armdata[[10]][[9]][[1]] # Missing two first oberservation
armdata[[10]][[9]][[1]][1,] <- armdata[[10]][[9]][[1]][3,]
armdata[[10]][[9]][[1]][2,] <- armdata[[10]][[9]][[1]][3,]


armdata[[11]][[9]][[1]] # Missing two first oberservation
armdata[[11]][[9]][[1]][1,] <- armdata[[11]][[9]][[1]][3,]
armdata[[11]][[9]][[1]][2,] <- armdata[[11]][[9]][[1]][3,]

armdata[[13]][[9]][[1]] # Missing four first oberservation
armdata[[13]][[9]][[1]][1,] <- armdata[[13]][[9]][[1]][5,]
armdata[[13]][[9]][[1]][2,] <- armdata[[13]][[9]][[1]][5,]
armdata[[13]][[9]][[1]][3,] <- armdata[[13]][[9]][[1]][5,]
armdata[[13]][[9]][[1]][4,] <- armdata[[13]][[9]][[1]][5,]


armdata[[14]][[9]][[2]] # Missing two first oberservations
armdata[[14]][[9]][[2]][1,] <- armdata[[14]][[9]][[2]][3,] 
armdata[[14]][[9]][[2]][2,] <- armdata[[14]][[9]][[2]][3,] 



xs <- c()
ys <- c()
zs <- c()
experiment <- c()
person <- c()
repetition <- c()

for (e in 1:16){
  for (p in 1:10){
    for (r in 1:10){
      xs <- c(xs,armdata[[e]][[p]][[r]][,1])
      ys <- c(ys,armdata[[e]][[p]][[r]][,2])
      zs <- c(zs,armdata[[e]][[p]][[r]][,3])
      experiment <- c(experiment,rep(e,100))
      person <- c(person, rep(p,100))
      repetition <- c(repetition, rep(r,100))
    }
  }
}
armdf <- data.frame(x = xs,y = ys,z = zs, experiment = factor(experiment), person = factor(person),repetition = factor(repetition))

z_sd_table <- matrix(data=NA,nrow = 16,ncol=10)

for (e in 1:16){
  for (p in 1:10){
    var_reps <- rep(NA,10)
    for (r in 1:10){
      var_reps[r] <- var(armdata[[e]][[p]][[r]][,3]) 
    }
    z_sd_table[e,p] <- sqrt(sum(99*var_reps)/(99*10))
  }
}


y_sd_table <- matrix(data=NA,nrow = 16,ncol=10)

for (e in 1:16){
  for (p in 1:10){
    var_reps <- rep(NA,10)
    for (r in 1:10){
      var_reps[r] <- var(armdata[[e]][[p]][[r]][,2]) 
    }
    y_sd_table[e,p] <- sqrt(sum(99*var_reps)/(99*10))
  }
}

Meanobs_sd_table <- vector(mode="list",length = 16)

for (e in 1:16){
  for (p in 1:10){
    for (r in 1:10){
      if (p==1 & r==1) person <- armdata[[e]][[p]][[r]]
      person <- person + armdata[[e]][[p]][[r]]
    }
  }
 Meanobs_sd_table[[e]] <- person/100
}
plot(x=armdf$x,y=armdf$z)

# Idea for 
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

normtest <- function(x){
  # What test to use
  test <- ks.test(x,"pnorm",mean(x),sd(x))
  return(test$p.value)
}

p_vals <- apply(data_dist,2,normtest)
p_vals
sum(p_vals >0.05)

p_vals_adj <- p.adjust(p_vals, method = "BH")
sum(p_vals_adj >0.05)



H0<-c(1:300)
p_value <- sort(p_vals_adj)
plot(H0,p_value,main="Adjusted P-values Shapiro test normality",col="blue")
lines(H0,rep(0.05,300),col="red",type="l",lty=2)
legend(x=0.9,legend=c("p-values","alpha=0.05"),col=c("blue","red"),lty=c(1,2))

min_index <- which.min(p_vals_adj)
max_index <- which.max(p_vals_adj)

par(mfrow=c(1,2))
qqnorm(data_dist[,min_index])
qqline(data_dist[,min_index])

qqnorm(data_dist[,max_index])
qqline(data_dist[,max_index])
par(mfrow=c(1,1))


hist(data_dist[,min_index])
plot(data_dist[,min_index])
which.max(data_dist[,min_index])
which.max(data_dist[-1121,min_index])
outliers <- c(1121,1281)
# Outlier 1 : 1121 Forsøg 12 person 3 rep 1'
# Outlier 2:  1281 Forsøg 13 person 9 rep 1'


data_dist_sorted <- data_dist[-outliers,]

p_vals_x <- apply(data_dist_sorted[,1:100],2,normtest)
p_vals_x
sum(p_vals_x >0.05)


p_vals_y <- apply(data_dist_sorted[,101:200],2,normtest)
p_vals_y
sum(p_vals_y >0.05)


p_vals_z <- apply(data_dist_sorted[,201:300],2,normtest)
p_vals_z
sum(p_vals_z >0.05)


p_vals_adj <- p.adjust(c(p_vals_x,p_vals_y,p_vals_z), method = "BH")


sum(p_vals_adj[1:100] >0.05)
sum(p_vals_adj[101:200] >0.05)
sum(p_vals_adj[201:300] >0.05)

X <- c(1:100)
p_val <- sort((p_vals_adj[1:100]))
plot(X,y=p_val,main="Adjusted P-values Kolmogorov-smirnov test normality",col="blue")
lines(X,sort(p_vals_adj[101:200]),col="red",type="p")
lines(X,sort(p_vals_adj[201:300]),col="green",type="p")
lines(X,rep(0.05,100),col="black",type="l",lty=2)
legend(x=0.95,legend=c("p-values_x","p-values_y","p-values_z","alpha=0.05"),col=c("blue","red","green","black"),lty=c(1,1,1,2))


