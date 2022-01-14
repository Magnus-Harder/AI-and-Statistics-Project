##
load("armdataCleaned.RData")

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

# Reformat data
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

#### First test, Summed norms of datapoints

 # Finding norms:
norms <- rep(0,1600)
for (i in 1:1600){
  for (j in 1:100){
    norms[i] <- norms[i] + norm(t(c(data_dist[i, j],data_dist[i, j+100],data_dist[i, j+200])),type="F")
  }
}

experiment <- as.factor(rep(c(1:16),each=100))
person <- as.factor(rep(rep(c(1:10),each=10),16))

#Two-way ANOVA
anova(lm(norms ~ person*experiment))

#Testing for normality, Sharpiro
shapiro.test(norms)

# Defining function
normtest <- function(x){
  # What test to use
  test <- ks.test(x,"pnorm",mean(x),sd(x))
  return(test$p.value)
}

# Kolmomgorov-Smirnoff test
normtest(norms)
ks.test(lm(norms ~ person*experiment)$residuals,"pnorm",mean(lm(norms ~ person*experiment)$residuals),sd(lm(norms ~ person*experiment)$residuals))
hist(norms)





