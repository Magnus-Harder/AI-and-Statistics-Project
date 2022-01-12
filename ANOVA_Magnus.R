load("armdataCleaned.RData")


anova(lm(armdata[[1]][[1]]~repetetion))

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

person <- as.factor(rep(rep(rep(c(1:10),each=10),16),300))
experiment <- as.factor(rep(rep(c(1:16),each=100),300))
anova(lm(c(data_dist)~person))
anova(lm(c(data_dist)~experiment))

kruskal.test(c(data_dist)~person)
kruskal.test(c(data_dist)~experiment)

length(experiment)

