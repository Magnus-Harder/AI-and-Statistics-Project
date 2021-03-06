McnemarSampleSize <- function(p10,p01,power,alpha){
  pdisc <- p10 + p01
  pdiff <- p10 - p01
  Z_alpha <- qnorm(1 - alpha / 2)
  Z_beta <- qnorm(power)
  n <- (( Z_alpha* sqrt(pdisc) +  Z_beta * sqrt(pdisc - pdiff ^ 2)) / pdiff) ^ 2
  n2 <- ( Z_alpha* sqrt(pdisc) +  Z_beta * sqrt(pdisc - pdiff ^ 2))^2    / (pdiff)^2
  n2 <- (Z_alpha^2* pdisc + Z_beta^2 *( pdisc - pdiff ^ 2))/ (pdiff)^2
  print(n2)
  return(n)
}
McnemarSampleSize <- function(p10,p01,n,alpha){
  pdisc <- p10 + p01
  pdiff <- p10 - p01
  Z_alpha <- qnorm(1 - alpha / 2)
  Z_beta <- qnorm(power)
  n <- (( Z_alpha* sqrt(pdisc) +  Z_beta * sqrt(pdisc - pdiff ^ 2)) / pdiff) ^ 2
  n2 <- ( Z_alpha* sqrt(pdisc) +  Z_beta * sqrt(pdisc - pdiff ^ 2))^2    / (pdiff)^2
  n2 <- (Z_alpha^2* pdisc + Z_beta^2 *( pdisc - pdiff ^ 2))/ (pdiff)^2
  print(n2)
  return(n)
}



McnemarPower <- function(p10,p01,n,alpha){
  pdisc <- p10 + p01
  pdiff <- p10 - p01
  Z_alpha <- qnorm(1 - alpha / 2)
  Z_beta = (sqrt(n*pdiff^2) - Z_alpha*sqrt(pdisc))/sqrt(-pdiff^2 + pdisc)
  
  power = pnorm(Z_beta)
  return(power)
}




McnemarTest <- function(A,B,y,alpha){
  ContingencyTable <- Contingency_table(A,B,y)
  n <-  length(y)
  n12 <- ContingencyTable[1,1]
  n21 <- ContingencyTable[2,1]
  Theta <- (n12-n21)/n
  Q <-(n^2 * (n+1) * (Theta+1) * (1-Theta) ) / ( n * (n12+n21) - (n12-n21)^2 )
  f = (Theta+1)/2 * (Q-1)
  g = (1-Theta)/2 * (Q-1)
  print(f)
  print(g)
  
  Theta_L <- 2*pbeta(q=alpha/2,shape1=f,shape2 = g)^-1
  Theta_U <- 2*pbeta(q=1-alpha/2,shape1=f,shape2 = g)-1
  p <- 2* pbinom(q=min(n12,n21),prob=1/2,size=n12+n21)
  return(c(p,Theta_L,Theta_U))
}


y_new <- rep(NA,3200)
y_new[1:1600] <- y
y_new[1601:3200] <- y_left

x_new <- matrix(nrow=3200,ncol=300)
x_new[1:1600,] <- data_dist
x_new[1601:3200,] <- data_left

X_all <- np$array(x_new)
y_all <- np$array(y_new)


ModelAll <- Model_left(X=X_all,y=y_all,n_splits=20,max_iter=1000)


sum(ModelAll[1:1600]==y)/1600
sum(ModelAll[1601:3200]==y)/1600 
sum(ModelAll[1601:3200]==y_left)/1600

prop.test(x=c(519,112),n=c(1600,1600))$p.value
power.prop.test(1600,519/1600,112/1600)
sum(ModelAll==y_new)

ModelAll

data_dist[1,201:300]
data_left[1,201:300]


mcnemar.test(Contingency_table(ModelAll[1601:3200],ModelRigthOnly[[2]],y_left))


