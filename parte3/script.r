library(leaps)
library(neuralnet)
library(hydroGOF)
library(arules)

#set.seed(4451) set a seed so that sample always gives the same train set
sample <- sample.int(n=nrow(dataset), size=floor(.75*nrow(dataset)),replace=F)
train <- dataset[sample, ]
test  <- dataset[-sample, ]

# formula variables should be changed after calling regsubsets(form, dataset, nvmax=8) after running conversion.r
formula <- FatigueLevel ~ Performance.MAMean + Performance.AEDMean + Performance.DDCMean + Performance.DMSMean
fatigue1 <- neuralnet(formula, train, hidden=c(7,6,5), lifesign = "FULL",linear.output = FALSE,threshold = 0.01)

# get a subset of test set (only de columns of the variables used in formula)
test.01 <- subset(test, select=c("Performance.MAMean", "Performance.AEDMean", "Performance.DDCMean", "Performance.DMSMean"))
fatigue1.resultados <- compute(fatigue1, test.01)
resultados <- data.frame(atual=test$FatigueLevel, previsao=fatigue1.resultados$net.result)
resultados$previsao <- round(resultados$previsao, digits=0)
rmse(c(test$FatigueLevel), c(resultados$previsao))

