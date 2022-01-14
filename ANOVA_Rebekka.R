load("armdataCleaned.RData")
curvemean <- function(data,idx){
  # call curvemean(armdata[[1]][[2]],1) to get curvemean for experiment 1, person 2 of the x-koordinates
  rowMeans(cbind(data[[1]][,idx],data[[2]][,idx],data[[3]][,idx],data[[4]][,idx],data[[5]][,idx],
                 data[[6]][,idx],data[[7]][,idx],data[[8]][,idx],data[[9]][,idx],data[[10]][,idx]))
}

curvemean2 <- function(data,idx){
  # call curvemean2(armdata[[1]],1) to get curvemean for experiment 1 over all persons of the x-koordinates
  rowMeans(cbind(curvemean(data[[1]],idx),curvemean(data[[2]],idx),curvemean(data[[3]],idx),curvemean(data[[4]],idx),curvemean(data[[5]],idx),
                 curvemean(data[[6]],idx),curvemean(data[[7]],idx),curvemean(data[[8]],idx),curvemean(data[[9]],idx),curvemean(data[[10]],idx)))
}

# Option 1: sum of norms from start to point
## Calculate norms
norms = rep(0,1600)
idx <- 1
for (e in 1:16){
  for (p in 1:10){
    for (r in 1:10){
      for (i in 2:100){
        norms[idx] = norms[idx] + norm(as.matrix(armdata[[e]][[p]][[r]][i,]-armdata[[e]][[p]][[r]][1,]), type="F")
      }
      idx <- idx +1
    }
  }
}

experiment <- as.factor(rep(c(1:16),each = 100))
person <- as.factor(rep(rep(c(1:10),each=10),16))

#Run two-way ANOVA
m <- lm(norms ~ person*experiment)
anova(m)
par(mfrow = c(2,2))
plot(m)

#KS test on residuals to test for normality
ks.test(m$residuals, "pnorm", mean(m$residuals), sd(m$residuals))
ks.test(norms, "pnorm", mean(norms), sd(norms))

"Since the p-value is higher than .05, we accept the null hypothesis. We do not have sufficient evidence to say that the sample data does not come from a normal distribution."

# Shapiro Wilks on residals to test for normality
shapiro.test(m$residuals)
shapiro.test(norms)

"From the output, the p-value < 0.05 implying that the distribution of the data are significantly different from normal distribution. In other words, we cannot assume the normality."

# Option 1.1: curve length
## Calculate curve lenghts
curve_lenghts = rep(0,1600)
idx <- 1
for (e in 1:16){
  for (p in 1:10){
    for (r in 1:10){
      for (i in 2:100){
        curve_lenghts[idx] = curve_lenghts[idx] + norm(as.matrix(armdata[[e]][[p]][[r]][i,]-armdata[[e]][[p]][[r]][i-1,]), type="F")
      }
      idx <- idx +1
    }
  }
}

experiment <- as.factor(rep(c(1:16),each = 100))
person <- as.factor(rep(rep(c(1:10),each=10),16))

#Run two-way ANOVA
m <- lm(curve_lenghts ~ person*experiment)
anova(m)
par(mfrow = c(2,2))
plot(m)

#KS test on residuals to test for normality
ks.test(m$residuals, "pnorm", mean(m$residuals), sd(m$residuals))
ks.test(norms, "pnorm", mean(norms), sd(norms))

"Since the p-value is higher than .05, we accept the null hypothesis. We do not have sufficient evidence to say that the sample data does not come from a normal distribution."

# Shapiro Wilks on residals to test for normality
shapiro.test(m$residuals)
shapiro.test(norms)

"From the output, the p-value < 0.05 implying that the distribution of the data are significantly different from normal distribution. In other words, we cannot assume the normality."

# Option 2: sum of distance between experiment and artificial baseline (Teoretisk kunne man bruge x_start = 0, x_end = 60, y_start = 0, z_start = 22.5)
"""x_start <- mean(c(curvemean2(armdata[[1]],1)[1],curvemean2(armdata[[2]],1)[1],curvemean2(armdata[[3]],1)[1],curvemean2(armdata[[4]],1)[1],curvemean2(armdata[[5]],1)[1],curvemean2(armdata[[6]],1)[1],
       curvemean2(armdata[[7]],1)[1],curvemean2(armdata[[8]],1)[1],curvemean2(armdata[[9]],1)[1],curvemean2(armdata[[10]],1)[1],curvemean2(armdata[[11]],1)[1],curvemean2(armdata[[12]],1)[1],
       curvemean2(armdata[[13]],1)[1],curvemean2(armdata[[14]],1)[1],curvemean2(armdata[[15]],1)[1],curvemean2(armdata[[16]],1)[1]))
x_end <- mean(c(curvemean2(armdata[[1]],1)[100],curvemean2(armdata[[2]],1)[100],curvemean2(armdata[[3]],1)[100],curvemean2(armdata[[4]],1)[100],curvemean2(armdata[[5]],1)[100],
                curvemean2(armdata[[6]],1)[100],curvemean2(armdata[[7]],1)[100],curvemean2(armdata[[8]],1)[100],curvemean2(armdata[[9]],1)[100],curvemean2(armdata[[10]],1)[100],
                curvemean2(armdata[[11]],1)[100],curvemean2(armdata[[12]],1)[100],curvemean2(armdata[[13]],1)[100],curvemean2(armdata[[14]],1)[100],curvemean2(armdata[[15]],1)[100],curvemean2(armdata[[16]],1)[100]))
y_start <- mean(c(curvemean2(armdata[[1]],2)[1],curvemean2(armdata[[2]],2)[1],curvemean2(armdata[[3]],2)[1],curvemean2(armdata[[4]],2)[1],curvemean2(armdata[[5]],2)[1],curvemean2(armdata[[6]],2)[1],
                  curvemean2(armdata[[7]],2)[1],curvemean2(armdata[[8]],2)[1],curvemean2(armdata[[9]],2)[1],curvemean2(armdata[[10]],2)[1],curvemean2(armdata[[11]],2)[1],curvemean2(armdata[[12]],2)[1],
                  curvemean2(armdata[[13]],2)[1],curvemean2(armdata[[14]],2)[1],curvemean2(armdata[[15]],2)[1],curvemean2(armdata[[16]],2)[1]))
z_start <- mean(c(curvemean2(armdata[[1]],3)[1],curvemean2(armdata[[2]],3)[1],curvemean2(armdata[[3]],3)[1],curvemean2(armdata[[4]],3)[1],curvemean2(armdata[[5]],3)[1],curvemean2(armdata[[6]],3)[1],
                  curvemean2(armdata[[7]],3)[1],curvemean2(armdata[[8]],3)[1],curvemean2(armdata[[9]],3)[1],curvemean2(armdata[[10]],3)[1],curvemean2(armdata[[11]],3)[1],curvemean2(armdata[[12]],3)[1],
                  curvemean2(armdata[[13]],3)[1],curvemean2(armdata[[14]],3)[1],curvemean2(armdata[[15]],3)[1],curvemean2(armdata[[16]],3)[1]))"""

x_start <- 0
x_end <- 60
y_start <- 0
z_start <- 22.5

## Calculate norms
norms2 = rep(0,1600)
idx <- 1
for (e in 1:16){
  for (p in 1:10){
    for (r in 1:10){
      for (i in 1:100){
        norms2[idx] = norms2[idx] + norm(as.matrix(armdata[[e]][[p]][[r]][i,]-c((x_end-x_start)/100*i,y_start,z_start)), type="F")
      }
      idx <- idx +1
    }
  }
}

#Run two-way ANOVA
m22 <- lm(norms2 ~ person*experiment)
anova(m22)
par(mfrow = c(2,2))
plot(m22)

#KS test on residuals to test for normality
ks.test(m22$residuals, "pnorm", mean(m22$residuals), sd(m22$residuals))
ks.test(norms2, "pnorm", mean(norms2), sd(norms2))

"Since the p-value is less than .05, we reject the null hypothesis. We do have sufficient evidence to say that the sample data does not come from a normal distribution."

# Shapiro Wilks on residals to test for normality
shapiro.test(m22$residuals)
shapiro.test(norms2)

"From the output, the p-value < 0.05 implying that the distribution of the data are significantly different from normal distribution. In other words, we cannot assume the normality."


# Option 2.1: sum of distance between experiment and baseline (16)
## calculate average for the control experiment
control <- cbind(curvemean2(armdata[[16]],1),curvemean2(armdata[[16]],2),curvemean2(armdata[[16]],3))

## Calculate norms
norms_diff = rep(0,1500)
idx <- 1
for (e in 1:15){
  for (p in 1:10){
    for (r in 1:10){
      for (i in 1:100){
        norms_diff[idx] = norms_diff[idx] + norm(as.matrix(armdata[[e]][[p]][[r]][i,]-control[i,]), type="F")
      }
      idx <- idx +1
    }
  }
}

experiment_diff <- as.factor(rep(c(1:15),each = 100))
person_diff <- as.factor(rep(rep(c(1:10),each=10),15))

#Run two-way ANOVA
m2 <- lm(norms_diff ~ person_diff*experiment_diff)
anova(m2)
par(mfrow = c(2,2))
plot(m2)

#KS test on residuals to test for normality
ks.test(m2$residuals, "pnorm", mean(m2$residuals), sd(m2$residuals))
ks.test(norms_diff, "pnorm", mean(norms_diff), sd(norms_diff))

"Since the p-value is less than .05, we reject the null hypothesis. We do have sufficient evidence to say that the sample data does not come from a normal distribution."

# Shapiro Wilks on residals to test for normality
shapiro.test(m2$residuals)
shapiro.test(norms_diff)

"From the output, the p-value < 0.05 implying that the distribution of the data are significantly different from normal distribution. In other words, we cannot assume the normality."

# Option 3: max(z)
#Find max(armdata[[e]][[p]][[r]][,3])

max_zid = rep(0,1600)
idx <- 1
for (e in 1:16){
  for (p in 1:10){
    for (r in 1:10){
      max_zid[idx] = which.max(armdata[[e]][[p]][[r]][,3])
      idx <- idx +1
    }
  }
}

## Option 3.1: get the norm of the max(z) position from start-position
norms_maxz = rep(NA,1600)

idx <- 1
for (e in 1:16){
  for (p in 1:10){
    for (r in 1:10){
      norms_maxz[idx] <- norm(t(armdata[[e]][[p]][[r]][max_zid[idx],]-armdata[[e]][[p]][[r]][1,]),type="F")
      idx <- idx +1
    }
  }
}

#Run two-way ANOVA
m3 <- lm(norms_maxz ~ person*experiment)
anova(m3)
par(mfrow = c(2,2))
plot(m3)

#KS test on residuals to test for normality
ks.test(m3$residuals, "pnorm", mean(m$residuals), sd(m$residuals))
ks.test(norms_maxz, "pnorm", mean(norms), sd(norms))

"Since the p-value is lower than .05, we reject the null hypothesis. We do have sufficient evidence to say that the sample data does not come from a normal distribution."

## Option 3.2: Elevation angle from start to max(z)-point



