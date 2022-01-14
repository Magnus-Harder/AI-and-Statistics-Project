library(reticulate)
source_python("Python/MLlearning.py")
np <- import("numpy",convert=FALSE)
load("DataMatrix.RData")

Contingency_table <- function(y1,y2,true) {
  f11 <- sum(y1==true & y2 == true)
  f01 <- sum(y1!=true & y2 == true)
  f10 <- sum(y1==true & y2 != true)
  f00 <- sum(y1!=true & y2 != true)
  return(matrix(c(f11,f01,f10,f00),nrow=2,ncol=2))
}
 



# Setting up data in numpy-arrays for training and testing of models
y <- np$array(rep(c(1:16),each=100))
X <- np$array(data_dist)

# Hyperparametter nr. of trees in the forrest (50,75,100,125,150,175,200)?
trees <- np$array(c(1,50,75,100,125,150,175,200))
 

#Calling testing function from python
MLR_model <- MLlearning2(X,y,n_trees=trees)

print(sum(MLR_model[[1]]==MLR_model[[3]]))
for (i in 1:length(trees)) print(sum(MLR_model[[2]][i,]==MLR_model[[3]]))

one_tree <- as.factor(MLR_model[[2]][1,])
fifty_trees <- as.factor(MLR_model[[2]][2,])
true <-MLR_model[[3]]

Contingency_table(one_tree,fifty_trees,MLR_model[[3]])

mc_test <-mcnemar.test(x=Contingency_table(one_tree,fifty_trees,MLR_model[[3]]),correct=FALSE)
mc_test$p.value


