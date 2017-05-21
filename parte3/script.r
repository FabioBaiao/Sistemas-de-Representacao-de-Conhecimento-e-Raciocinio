library(leaps)
library(neuralnet)
library(hydroGOF)
library(arules)

#dataset <- read.csv("~/Desktop/6th_semester/srcr/project/projecto_rna/discrExaustao.csv")
#set.seed(11) # set a seed so that sample always gives the same train set
#sample <- sample.int(n=nrow(dataset), size=floor(.75*nrow(dataset)),replace=F)
#train <- dataset[sample, ]
#test  <- dataset[-sample, ]

#sigmoid <- function(x) {1/(1+exp(-x))}
# formula variables should be changed after calling regsubsets(form, dataset, nvmax=8) after running conversion.r
#formula <- FatigueLevel ~ Performance.MAMean + Performance.AEDMean + Performance.DDCMean + Performance.DMSMean
#formulaFL <- FatigueLevel ~ Performance.KDTMean + Performance.MAMean + Performance.AEDMean + Performance.DDCMean + Performance.DMSMean
formulaT <- Performance.Task ~ Performance.KDTMean + Performance.MAMean + Performance.DDCMean + Performance.DMSMean + Performance.MVMean
task1 <- neuralnet(formulaT, train, hidden=c(7,3), lifesign = "FULL",algorithm="rprop+",linear.output = FALSE,threshold = 0.03)

# get a subset of test set (only de columns of the variables used in formula)
test.01 <- subset(test, select=c("Performance.KDTMean","Performance.MAMean", "Performance.DDCMean", "Performance.DMSMean", "Performance.MVMean"))
task1.resultados <- compute(task1, test.01)
resultados <- data.frame(atual=test$Performance.Task, previsao=(task1.resultados$net.result*2)+1)
resultados$previsao <- round(resultados$previsao, digits=0)
resultados$atual <- round(resultados$atual, digits=0)
rmse(c(test$Performance.Task), c(resultados$previsao))
