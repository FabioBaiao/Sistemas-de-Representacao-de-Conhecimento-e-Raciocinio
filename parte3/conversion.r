library(arules)
library(leaps)
###
#
# SHOULD WE RUN regsubsets (for selecting the most meaningful variables) BEFORE OR AFTER RUNNING 
# THIS SCRIPT???? I TRIED BOTH WAYS, THERE WAS NO RELEVANT DIFFERENCE
#
###

# change this to your path
dataset <- read.csv("~/Desktop/6th_semester/srcr/project/projecto_rna/exaustao.csv")

# after using weka or running plot(density(dataset$<column>)) to see the distribution of each
# variable, we discretize each one accordingly
x1 <- as.numeric(discretize(dataset$Performance.KDTMean,method="frequency",categories=1,labels=1:1))
x2 <- as.numeric(discretize(dataset$Performance.MAMean,method="frequency",categories=2,labels=1:2))
x3 <- as.numeric(discretize(dataset$Performance.MVMean,method="frequency",categories=1,labels=1:1))
x4 <- as.numeric(discretize(dataset$Performance.TBCMean,method="frequency",categories=2,labels=1:2))
x5 <- as.numeric(discretize(dataset$Performance.DDCMean,method="frequency",categories=3,labels=1:3))
x6 <- as.numeric(discretize(dataset$Performance.DMSMean,method="frequency",categories=2,labels=1:2))
x7 <- as.numeric(discretize(dataset$Performance.AEDMean,method="frequency",categories=3,labels=1:3))
x8 <- as.numeric(discretize(dataset$Performance.ADMSLMean,method="frequency",categories=4,labels=1:4))

# uncomment the next lines and change category and labels to use other exhaustion scales
#f <- as.numeric(discretize(dataset$FatigueLevel,method="frequency",categories=5,labels=1:5))
#dataset$FatigueLevel <- f

dataset$Performance.KDTMean <- x1
dataset$Performance.MAMean <- x2
dataset$Performance.MVMean <- x3
dataset$Performance.TBCMean <- x4
dataset$Performance.DDCMean <- x5
dataset$Performance.DMSMean <- x6
dataset$Performance.AEDMean <- x7
dataset$Performance.ADMSLMean <- x8

# this just maps work, office and programming to 1,2 and 3 (not sure about the order here)
dataset$Performance.Task <- as.numeric(dataset$Performance.Task)

# formula to use when using regsubsets
form <- FatigueLevel ~ Performance.KDTMean + Performance.MAMean + Performance.MVMean + Performance.DDCMean + Performance.AEDMean + Performance.DMSMean + Performance.ADMSLMean + Performance.TBCMean

