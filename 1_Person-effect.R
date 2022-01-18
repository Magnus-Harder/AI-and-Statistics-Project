load("armdataCleaned.RData")

## Calculate Average Initial velocity
velocity <- rep(0,1599)
experiment <- rep(NA, 1599)
person <- rep(NA, 1599)
idx <- 1
for (e in 1:16){
  for (p in 1:10){
    for (r in 1:10){
      if(e==5 & p ==2 & r==7) next
      for (i in 2:100){
        velocity[idx] = velocity[idx] + (norm(as.matrix(armdata[[e]][[p]][[r]][i,]-armdata[[e]][[p]][[r]][1,]), type="F")/(i-1))
      }
      experiment[idx] <- e
      person[idx] <- p
      idx <- idx +1
    }
  }
}

experiment <- as.factor(experiment)
person <- as.factor(person)

#Find average velocity approximation
velocity <- velocity/99

#Run two-way ANOVA
m <- lm(velocity ~ person*experiment)
summary(m)
anova(m)
par(mfrow = c(1,2))
plot(m)

#KS test on residuals to test for normality
ks.test(m$residuals, "pnorm", mean(m$residuals), sd(m$residuals))

# Shapiro Wilks on residals to test for normality
shapiro.test(m$residuals)

# Visual inspection of the distribution of the 
par(mfrow = c(1,2))
hist(m$residuals, prob = TRUE, col =2, breaks = 20, main = "Histogram of the Residuals of the interaction model", xlab = "Model residuals")
curve(dnorm(x, mean=mean(m$residuals), sd=sd(m$residuals)),col="darkblue", lwd=2, add=TRUE, yaxt="n")
legend("topleft", c("Residuals", "Normal distribution"), lwd = c(4,2), col = c(2,"darkblue"))

plot(ecdf(m$residuals),col=2, main = "Cumulative Density Function comparison", ylab = "CDF", xlab = "Data")
curve(pnorm(x, mean(m$residuals), sd(m$residuals)),add=TRUE, col = 1, lty = 2)
legend(-0.2, 0.98, c("Emprical CDF of Residuals", "CDF of Normal distribution"), lwd = 1, lty = c(1,2), col = c(2,1), cex = 0.9)

# Draw what this looks like
library(rgl)
r3dDefaults$windowRect <- c(0,50, 1500, 1000)

start_cyl <- cylinder3d(cbind(0, 0, seq(0, 10, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
target_cyl <- cylinder3d(cbind(60, 0, seq(0, 10, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl1 <- cylinder3d(cbind(0, 0, 10 + seq(0, 12.5, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl2 <- cylinder3d(cbind(60, 0, 10 + seq(0, 12.5, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl3 <- cylinder3d(cbind(15, 0, seq(0, 35, length = 35)), radius = c(3,3,3), sides = 10, closed = -2)
shade3d(addNormals(subdivision3d(start_cyl)), col = 'darkred')
shade3d(addNormals(subdivision3d(target_cyl)), col = 'darkgreen')
shade3d(addNormals(subdivision3d(cyl1)), col = 'pink')
shade3d(addNormals(subdivision3d(cyl2)), col = 'pink', alpha = 0.5)
shade3d(addNormals(subdivision3d(cyl3)), col = 'lightblue')
surface3d(c(-7, 67), c(-20, 20), matrix(0, 2, 2), col = "brown", alpha = 0.9, specular = "black")
lines3d(armdata[[3]][[1]][[1]], col =2)
for (i in 2:100){
  arrow3d(armdata[[3]][[1]][[1]][1,],armdata[[3]][[1]][[1]][i,], type= "lines", s = 1/50, width = 1/20, thickness = 0.3*width)
}
rgl.viewpoint(zoom = .8)