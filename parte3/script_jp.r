library(leaps)
library(neuralnet)
library(hydroGOF)
library(arules)

# Change this to your file name
FILE <- "C:\\Users\\joaop\\Documents\\GitHub\\srcr_assignment\\parte3\\original_data\\exaustao.csv"
dataset <- read.csv(FILE)

# DATA ADAPTATION
dataset$Performance.Task[dataset$Performance.Task == "Work"] <- 1
dataset$Performance.Task[dataset$Performance.Task == "programming"] <- 2
dataset$Performance.Task[dataset$Performance.Task == "office"] <- 3

# STATISTICAL ANALISYS
rsubsFL <- regsubsets(x = FatigueLevel ~ Performance.KDTMean + Performance.MAMean + Performance.MVMean + Performance.TBCMean + Performance.DDCMean + Performance.DMSMean + Performance.AEDMean + Performance.ADMSLMean, data = dataset, nvmax = 8)
rsubsT <- regsubsets(x = Performance.Task ~ Performance.KDTMean + Performance.MAMean + Performance.MVMean + Performance.TBCMean + Performance.DDCMean + Performance.DMSMean + Performance.AEDMean + Performance.ADMSLMean, data = dataset, nvmax = 8)
rsubsT_FL <- regsubsets(x = Performance.Task + FatigueLevel ~ Performance.KDTMean + Performance.MAMean + Performance.MVMean + Performance.TBCMean + Performance.DDCMean + Performance.DMSMean + Performance.AEDMean + Performance.ADMSLMean, data = dataset, nvmax = 8)
# NEURAL NETWORK DEFINITION
