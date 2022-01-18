Contingency_table <- function(y1,y2,true) {
  f11 <- sum(y1==true & y2 == true)
  f10 <- sum(y1==true & y2 != true)
  f01 <- sum(y1!=true & y2 == true)
  f00 <- sum(y1!=true & y2 != true)
  return(matrix(c(f11,f10,f01,f00),nrow=2,ncol=2))
}
McnemarPower <- function(p10,p01,n,alpha){
  pdisc <- p10 + p01
  pdiff <- p10 - p01
  Z_alpha <- qnorm(1 - alpha / 2)
  Z_beta = (sqrt(n*pdiff^2) - Z_alpha*sqrt(pdisc))/sqrt(-pdiff^2 + pdisc)
  
  power = pnorm(Z_beta)
  return(power)
}
