## Load data
load("armdataCleaned.RData")

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

 # Reformat to matrix
xs <- c()
ys <- c()
zs <- c()
experiment <- c()
person <- c()
repetition <- c()

for (e in 1:16){
  for (p in 1:10){
    for (r in 1:10){
      xs <- c(xs,armdata[[e]][[p]][[r]][,1])
      ys <- c(ys,armdata[[e]][[p]][[r]][,2])
      zs <- c(zs,armdata[[e]][[p]][[r]][,3])
      experiment <- c(experiment,rep(e,100))
      person <- c(person, rep(p,100))
      repetition <- c(repetition, rep(r,100))
    }
  }
}
armdf <- data.frame(x = xs,y = ys,z = zs, experiment = factor(experiment), person = factor(person),repetition = factor(repetition))

# Reformat data #2
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



#___________________________________________________________ QUESTION 1 ________________________________________________________________________#

#### First test, Summed norms of datapoints from startin points

# Finding norms:
norms <- rep(0,1600)
for (i in 1:1600){
  for (j in 1:100){
    norms[i] <- norms[i] + norm(t(c(data_dist[i, j],data_dist[i, j+100],data_dist[i, j+200])-c(data_dist[i,1],data_dist[i,101],data_dist[i,201])),type="F")
  }
}

experiment <- as.factor(rep(c(1:16),each=100))
person <- as.factor(rep(rep(c(1:10),each=10),16))

# Two-way ANOVA
anova(lm(norms ~ person*experiment))
anova(lm(norms ~ person))

# Testing for normality, Sharpiro
shapiro.test(norms)
shapiro.test(lm(norms ~ person)$residuals)


# Defining function
normtest <- function(x){
  # What test to use
  test <- ks.test(x,"pnorm",mean(x),sd(x))
  return(test$p.value)
}

# Kolmomgorov-Smirnoff test
m <- lm(norms ~ person*experiment)
ks.test(norms,"pnorm",mean(norms),sd(norms))
ks.test(m$residuals,"pnorm",mean(m$residuals),sd(m$residuals))
hist(norms)
par(mfrow = c(2,2))
plot(m)












#### Second test: Max(z)

maxz <- rep(NA,1600)
for (i in 1:1600){
    maxz[i] <-  max(data_dist[i, 200:300])
}
maxz

# Two-way ANOVA
anova(lm(maxz ~ person))

# Kolmomgorov-Smirnoff test
m2 <- lm(maxz ~ person*experiment)
ks.test(maxz,"pnorm",mean(norms),sd(norms))
ks.test(m2$residuals,"pnorm",mean(m2$residuals),sd(m2$residuals))
hist(maxz)
par(mfrow = c(2,2))
plot(m2)


#___________________________________________________________ QUESTION 2 _________________________________________________________#






#___________________________________________________________ Left Hand Data ___________________________________________________________#

lefthanddata <- matrix(nrow=1600,ncol = 300)

# Creating left-hand data

idx <- 1 
for (e in 1:16){
  for (p in 1:10){
    for (r in 1:10){
      lefthanddata[idx,1:100] <- max(armdata[[e]][[p]][[r]][,1]) - rev(armdata[[e]][[p]][[r]][,1])
      lefthanddata[idx,101:200] <- armdata[[e]][[p]][[r]][,2]
      lefthanddata[idx,201:300] <- rev(armdata[[e]][[p]][[r]][,3])
      idx <- idx + 1
    }
  }
}





experiments <- rep(c(13,14,15,10,11,12,7,8,9,4,5,6,1,2,3),100)


lefthanddata2 <- matrix(nrow=1600,ncol = 300)
#Experiment 1:3
lefthanddata2[201:300,] <- lefthanddata[1401:1500, ]
lefthanddata2[101:200,] <- lefthanddata[1301:1400, ]
lefthanddata2[1:100,] <- lefthanddata[1201:1300, ]

#Experiment 4:6
lefthanddata2[501:600,] <- lefthanddata[1101:1200, ]
lefthanddata2[401:500,] <- lefthanddata[1001:1100, ]
lefthanddata2[301:400,] <- lefthanddata[901:1000, ]

#Experiment 7:9
lefthanddata2[801:900,] <- lefthanddata[801:900, ]
lefthanddata2[701:800,] <- lefthanddata[701:800, ]
lefthanddata2[601:700,] <- lefthanddata[601:700, ]

#Experiment 10:12
lefthanddata2[1101:1200,] <- lefthanddata[501:600, ]
lefthanddata2[1001:1100,] <- lefthanddata[401:500, ]
lefthanddata2[901:1000,] <- lefthanddata[301:400, ]

#Experiment 13:15
lefthanddata2[1401:1500,] <- lefthanddata[201:300, ]
lefthanddata2[1301:1400,] <- lefthanddata[101:200, ]
lefthanddata2[1201:1300,] <- lefthanddata[1:100, ]

#Experiment 16
lefthanddata2[1501:1600,] <- lefthanddata[1501:1600,]

save(lefthanddata2,file = "lefthanddata.RData")



### 3.d plotting
#Forsøg 1 person 1
load("lefthanddata.RData")



library(rgl)

r3dDefaults$windowRect <- c(0,50, 1500, 1000)

start_cyl <- cylinder3d(cbind(0, 0, seq(0, 10, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
target_cyl <- cylinder3d(cbind(60, 0, seq(0, 10, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl1 <- cylinder3d(cbind(0, 0, 10 + seq(0, 12.5, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl2 <- cylinder3d(cbind(60, 0, 10 + seq(0, 12.5, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl3 <- cylinder3d(cbind(15, 0, seq(0, 35, length = 35)), radius = c(3,3,3), sides = 10, closed = -2)
shade3d(addNormals(subdivision3d(start_cyl)), col = 'darkgreen')
shade3d(addNormals(subdivision3d(target_cyl)), col = 'darkred')
shade3d(addNormals(subdivision3d(cyl1)), col = 'pink')
shade3d(addNormals(subdivision3d(cyl2)), col = 'pink', alpha = 0.5)
#shade3d(addNormals(subdivision3d(cyl3)), col = 'lightblue')
surface3d(c(-7, 67), c(-20, 20), matrix(0, 2, 2), col = "brown", alpha = 0.9, specular = "black")
for (i in 1501:1600){
  lines3d(cbind(data_dist[i,1:100],rep(0,100),data_dist[i,201:300]),col=i)
}
rgl.viewpoint(theta=0,phi=-90)

# Forsøg 1 person 1, venstrehånedet



surface3d(c(-7, 67), c(-20, 20), matrix(0, 2, 2), col = "brown", alpha = 0.9, specular = "black")
start_cyl <- cylinder3d(cbind(0, 0, seq(0, 10, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
target_cyl <- cylinder3d(cbind(60, 0, seq(0, 10, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl1 <- cylinder3d(cbind(0, 0, 10 + seq(0, 12.5, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl2 <- cylinder3d(cbind(60, 0, 10 + seq(0, 12.5, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl3 <- cylinder3d(cbind(15, 0, seq(0, 35, length = 35)), radius = c(3,3,3), sides = 10, closed = -2)
shade3d(addNormals(subdivision3d(start_cyl)), col = 'darkgreen')
shade3d(addNormals(subdivision3d(target_cyl)), col = 'darkred')
shade3d(addNormals(subdivision3d(cyl1)), col = 'pink')
shade3d(addNormals(subdivision3d(cyl2)), col = 'pink', alpha = 0.5)
#shade3d(addNormals(subdivision3d(cyl3)), col = 'lightblue')
for (i in 1501:1600){
  lines3d(cbind(data_left[i,1:100],rep(0,100),data_left[i,201:300]),col=i)
}
rgl.viewpoint(theta=0,phi=-90)








exp <- rep(c(13,14,15,10,11,12,7,8,9,4,5,6,1,2,3),100)

data_left <- matrix(nrow=1600,ncol = 300)


idx <- 1 
for (e in 1:16){
  for (p in 1:10){
    for (r in 1:10){
      data_left[idx,1:100] <- armdata[[e]][[p]][[r]][,1]
      data_left[idx,101:200] <- armdata[[e]][[p]][[r]][,2]
      data_left[idx,201:300] <- rev(armdata[[e]][[p]][[r]][,3])
      idx <- idx + 1
    }
  }
}
data_left[1]

for (i in 1:10){
  lines3d(data_left[i,],col=i)
}



data_left[1]






