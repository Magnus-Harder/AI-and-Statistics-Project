library(reticulate)
source_python("Python/MLlearning.py")
source("functions.R")
np <- import("numpy",convert=FALSE)
load("Data/DataMatrix.RData")
y <- np$array(rep(c(1:16),each=100))
X <- np$array(data_dist)
trees <- np$array(c(50,75,100,125,150))


MLR_model_xyz <- MLlearning(np$array(data_dist),y,n_trees=trees,max_iter=1000,n_splits_outer=10,n_splits_inner=5)
MLR_model_xy <- MLlearning(np$array(data_dist[,c(1:100,101:200)]),y,n_trees=trees,max_iter=1000,n_splits_outer=10,n_splits_inner=5)
MLR_model_xz <- MLlearning(np$array(data_dist[,c(1:100,201:300)]),y,n_trees=trees,max_iter=1000,n_splits_outer=10,n_splits_inner=5)
MLR_model_yz <- MLlearning(np$array(data_dist[,c(101:200,201:300)]),y,n_trees=trees,max_iter=1000,n_splits_outer=10,n_splits_inner=5)

y_true <- rep(c(1:16),each=100)

# MLR evaluaation
mc_test_xy_xyz <-mcnemar.test(x=Contingency_table(MLR_model_xyz[[1]],MLR_model_xy[[1]],y_true),correct=FALSE)
table <- Contingency_table(MLR_model_xyz[[1]],MLR_model_xy[[1]],y_true)
table
mc_test_xy_xyz$p.value
McnemarPower(table[2,1]/1600,table[1,2]/1600,1600,0.05)


mc_test_xz_xyz <-mcnemar.test(x=Contingency_table(MLR_model_xyz[[1]],MLR_model_xz[[1]],y_true),correct=FALSE)
table <- Contingency_table(MLR_model_xyz[[1]],MLR_model_xz[[1]],y_true)
table 
mc_test_xz_xyz$p.value
McnemarPower(table[2,1]/1600,table[1,2]/1600,1600,0.05)

mc_test_yz_xyz <-mcnemar.test(x=Contingency_table(MLR_model_xyz[[1]],MLR_model_yz[[1]],y_true),correct=FALSE)
table <- Contingency_table(MLR_model_xyz[[1]],MLR_model_yz[[1]],y_true)
table
mc_test_yz_xyz$p.value
McnemarPower(table[2,1]/1600,table[1,2]/1600,1600,0.05)

# RF evaluaation
mc_test_xy_xyz <-mcnemar.test(x=Contingency_table(MLR_model_xyz[[2]],MLR_model_xy[[2]],y_true),correct=FALSE)
table <- Contingency_table(MLR_model_xyz[[2]],MLR_model_xy[[2]],y_true)
table 
mc_test_xy_xyz$p.value
McnemarPower(table[2,1]/1600,table[1,2]/1600,1600,0.05)

mc_test_xz_xyz <-mcnemar.test(x=Contingency_table(MLR_model_xyz[[2]],MLR_model_xz[[2]],y_true),correct=FALSE)
table <-Contingency_table(MLR_model_xyz[[2]],MLR_model_xz[[2]],y_true)
table 
mc_test_xz_xyz$p.value
McnemarPower(table[2,1]/1600,table[1,2]/1600,1600,0.05)

mc_test_yz_xyz <-mcnemar.test(x=Contingency_table(MLR_model_xyz[[2]],MLR_model_yz[[2]],y_true),correct=FALSE)
table <- Contingency_table(MLR_model_xyz[[2]],MLR_model_yz[[2]],y_true)
table 
mc_test_yz_xyz$p.value
McnemarPower(table[2,1]/1600,table[1,2]/1600,1600,0.05)


