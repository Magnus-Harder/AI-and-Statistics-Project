library(reticulate)
source("functions.R")
np <- import("numpy",convert = FALSE)
source_python("Python/Bias.py")
load("DataMatrix.RData")
load("lefthanddata.RData")
y <- rep(1:16,each=100)
X <- np$array(data_dist)
X_left <- np$array(lefthanddata2)
y_left <- rep(c(13,14,15,10,11,12,7,8,9,4,5,6,1,2,3,16),each=100)

ModelRigthOnly <- Bias_left(X=X,y=np$array(y),X_left=X_left)

"Independent"

sum(ModelRigthOnly [[1]]==y)/1600
sum(ModelRigthOnly [[2]]==y)/1600


prop.test(x=c(864,186),n=c(1600,1600))
test$p.value
power.prop.test(n=1600,p1=0.540000,p2=186/1600)




idx <- 0
X_new <- data_dist
for (i in 1:160){
  for (i in 1:5){
    X_new[idx+i*2,] <- lefthanddata2[idx+i*2,]
  }
  idx <- idx + 10
}

ModelMix <- Model_left(X=np$array(X_new),y=np$array(y),n_splits=10,max_iter=1000)

idx <- 0
idx_vals <- 1
idx_left <- rep(NA,800)
idx_rigth <- rep(NA,800)
for (i in 1:160){
  for (i in 1:5){
    idx_left[idx_vals] <- idx + i*2
    idx_rigth[idx_vals] <- idx + i*2-1
    idx_vals <- idx_vals + 1
  }
  idx <- idx + 10
}


sum(ModelMix[idx_left]==y[idx_left])/800
sum(ModelMix[idx_rigth]==y[idx_rigth])/800
sum(ModelMix==y)/1600

prop.test(x=c(322,327),n=c(800,800))

