# Sun Jan  9 18:33:58 2022 ------------------------------
load("armdata.RData")

#' How to Call the Data (Using list reqiures dubble [[]]  indexing)
#' First layer is Experiment, there is a total of 16 different experiemnts in first layer
#' Second layer is Persons, There is 10 different persons 
#' Third layer is repetetions, there is 10 repetetions for each person in each experiemtn
#' Totalt number of experiments is thus 16 · 10 · 10 = 1600
#' Callling Experiment 4, Person 3, Repetetion 8, would thus be armdata[[4]][[3]][[8]]


Exp_1 <- armdata[[1]]
Exp_2 <- armdata[[2]]

Exp_1_per_1 <- Exp_1[[1]]
Exp_2_per_1 <- Exp_1[[1]]
Exp_1_per_1_rep_1 <- Exp_1_per_1[[1]] # or armdata[[1]][[1]][[1]]
Exp_1_per_1_rep_2 <- Exp_1_per_1[[2]] # or armdata[[1]][[1]][[2]]






armdata[[1]]
one[[1]]
two <- one[[1]]
two[[1]]


one[[1]][[3]]
