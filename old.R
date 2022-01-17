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

