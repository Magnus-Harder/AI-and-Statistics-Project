library(reticulate)
source_python("Python/MultinomialLogisticRegression.py")
np <- import("numpy",convert=FALSE)
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

y <- np$array(rep(c(1:16),each=100))
X <- np$array(data_dist)
Nr_classes <- 16
MLR_model <- MultiLogReg(X,y,Nr_classes)
MLR_model$train(1,1000)
