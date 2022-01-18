library(reticulate)
source_python("Python/MLlearning.py")
source("functions.R")
np <- import("numpy",convert=FALSE)
load("Data/DataMatrix.RData")


# Setting up data in numpy-arrays for training and testing of models
y <- np$array(rep(c(1:16),each=100))
X <- np$array(data_dist)

# Hyperparametter nr. of trees in the forrest (50,75,100,125,150,175,200)?
trees <- np$array(c(50,75,100,125,150))
 

#Calling testing function from python
MLR_model <- MLlearning(X,y,n_trees=trees,max_iter=1000,n_splits_outer=10,n_splits_inner=10)


MLR<- as.factor(MLR_model[[1]])
RF <- as.factor(MLR_model[[2]])
Optimal_trees <- MLR_model[[3]]
Optimal_trees
y_true <- rep(c(1:16),each=100)

table <- Contingency_table(MLR,RF,y_true)
table

mc_test <-mcnemar.test(x=Contingency_table(MLR,RF,y_true),correct=FALSE)
mc_test$p.value

p10 <- table[2,1]/1600
p01 <- table[1,2]/1600
McnemarPower(p10=p10,p01=p01,n=1600,alpha = 0.05)

sum(MLR==y_true)
sum(RF==y_true)
