# Sun Jan  9 18:33:58 2022 ------------------------------
load("armdata.RData")

#' How to Call the Data (Using list requires double [[]]  indexing)
#' First layer is Experiment, there is a total of 16 different experiments in first layer
#' Second layer is Persons, There is 10 different persons 
#' Third layer is Repetitions, there is 10 repetitions for each person in each experiment
#' Total number of experiments is thus 16 · 10 · 10 = 1600
#' Calling Experiment 4, Person 3, Repetition 8, would thus be armdata[[4]][[3]][[8]]


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
rm(list = c("e","p","r","xs","ys","zs","experiment","person","repetition"))

color_person = c("#9E0142","#D53E4F","#F46D43","#FDAE61","#FEE08B",
                 "#E6F598","#ABDDA4","#66C2A5","#3288BD","#5E4FA2")

### 1. Initial Analysis
boxplot(armdf$x ~ armdf$person, data = armdf[(armdf$experiment == 1), ], col = 2:11)
boxplot(armdf$y ~ armdf$person, data = armdf[(armdf$experiment == 1), ], col = 2:11)
boxplot(armdf$z ~ armdf$person, data = armdf[(armdf$experiment == 1), ], col = 2:11)

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

for (j in 1:16){
  plot(curvemean(armdata[[j]][[1]],3) ~ curvemean(armdata[[j]][[1]],1), type="l", main = paste("Banekurve for eksperiment",j) , xlab = "gennemsnitlig X-koordinat", ylab = "gennemsnitlig z-koordinat", col = color_person[1], ylim = c(20,max(armdf[(armdf$experiment == j),3],na.rm = TRUE)))
  for (i in 2:10){
    lines(curvemean(armdata[[j]][[i]],1),curvemean(armdata[[j]][[i]],3), col = color_person[i])
  }
  legend("topleft", c("Person 1","Person 2","Person 3","Person 4","Person 5","Person 6","Person 7","Person 8","Person 9","Person 10"),col = color_person, lty = 1,cex=.4)
}

#outlier (dropped cylinder)
plot(armdata[[5]][[2]][[7]][,3] ~ armdata[[5]][[2]][[7]][,1], type="l", col = color_person[1])

for (j in 1:15){
  plot(curvemean(armdata[[j]][[1]],3)-curvemean(armdata[[16]][[1]],3) ~ curvemean(armdata[[j]][[1]],1), type="l", main = paste("Diff. baseline og banekurven for eks.",j) , xlab = "gennemsnitlig X-koordinat", ylab = "gennemsnitlig diff z-koordinat", col = color_person[1], ylim = c(-2,max(armdf[(armdf$experiment == j),3]- armdf[(armdf$experiment == 16),3],na.rm = TRUE)))
  for (i in 2:10){
    lines(curvemean(armdata[[j]][[i]],1),curvemean(armdata[[j]][[i]],3)-curvemean(armdata[[16]][[i]],3), col = color_person[i])
  }
  legend("topleft", c("Person 1","Person 2","Person 3","Person 4","Person 5","Person 6","Person 7","Person 8","Person 9","Person 10"),col = color_person, lty = 1,cex=.4)
}

### 3.d plotting
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
for (i in 1:10){
  lines3d(cbind(curvemean(armdata[[3]][[i]],1),curvemean(armdata[[3]][[i]],2),curvemean(armdata[[3]][[i]],3)), col = color_person[i])
}
legend3d("topleft", legend = paste('Person ', c(1:10)), lty = 1, col = color_person, cex=2.5, inset=c(0.02))
rgl.viewpoint(zoom = .8)

colors_exp.no <- c("#E41A1C","#874F6F","#3881B0","#449B75","#56A255","#7E6E85","#AC5782","#E3712B",
                   "#FFA10D","#FFE528","#E1C62F","#B16C29","#C66764","#F17EB4","#CB8CAD","#999999")
start_cyl <- cylinder3d(cbind(0, 0, seq(0, 10, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
target_cyl <- cylinder3d(cbind(60, 0, seq(0, 10, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl1 <- cylinder3d(cbind(0, 0, 10 + seq(0, 12.5, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl2 <- cylinder3d(cbind(60, 0, 10 + seq(0, 12.5, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
#cyl3 <- cylinder3d(cbind(30, 0, seq(0, 20, length = 10)), radius = c(3,3,3), sides = 10, closed = -2)
shade3d(addNormals(subdivision3d(start_cyl)), col = 'darkred')
shade3d(addNormals(subdivision3d(target_cyl)), col = 'darkgreen')
shade3d(addNormals(subdivision3d(cyl1)), col = 'pink')
shade3d(addNormals(subdivision3d(cyl2)), col = 'pink', alpha = 0.5)
#shade3d(addNormals(subdivision3d(cyl3)), col = 'lightblue')
surface3d(c(-7, 67), c(-20, 20), matrix(0, 2, 2), col = "brown", alpha = 0.9, specular = "black")
for (i in 1:16){
  lines3d(cbind(curvemean2(armdata[[i]],1),curvemean2(armdata[[i]],2),curvemean2(armdata[[i]],3)), col = colors_exp.no[i])
}
legend3d("topleft", legend = paste('Experiment', c(1:16)), lty = 1, col = colors_exp.no, cex=2.5, inset=c(0.02))
rgl.viewpoint(zoom = .8)



## Tabt cylinder
lines3d(armdata[[5]][[2]][[7]])
