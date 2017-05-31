################################################################################
##
## MicrosoftML.
## written by Y.Nakahashi 
## 2017/5/31
##
################################################################################

################################################################################
### environmental settings
################################################################################
## working directory
wk_Dir <- "C:/Users/Administrator/Desktop/MicrosoftML"
setwd(wk_Dir)

## install & load libraries
library(MicrosoftML)
library(RevoScaleR)
library(lattice)


################################################################################
### data loading
################################################################################
# Read the data into a data frame in memory
mortXdf <- file.path(rxGetOption("sampleDataDir"), "mortDefaultSmall")
mortDF  <- rxDataStep(mortXdf)

# Create a logical TRUE/FALSE variable
mortDF$default <- mortDF$default == 1

# Divide the data into train and test data sets
set.seed(123)
isTest    <- rnorm(nrow(mortDF)) > 0
mortTest  <- mortDF[isTest, ]
mortTrain <- mortDF[!isTest, ]

# Get information on Data
rxGetInfo(mortTrain, getVarInfo = TRUE)
rxGetInfo(mortTest)


################################################################################
### do classification
################################################################################
binaryFormula <- default ~ creditScore + houseAge + yearsEmploy + ccDebt + year

# rxLogisticRegression
logitModel <- rxLogisticRegression(binaryFormula, data = mortTrain)
logitModel <- rxLogisticRegression(binaryFormula, data = mortTrain, 
                                   showTrainingStats = TRUE)
summary(logitModel)
logitScore <- rxPredict(logitModel, data = mortTest, 
                        extraVarsToWrite = "default")

# draw ROC Curve
# AUC = 0.90
rxRocCurve(actualVarName = "default", predVarNames = "Probability", 
           data = logitScore)

# rxFastTrees
fastTreeModel <- rxFastTrees(binaryFormula, data = mortTrain, 
                             numTrees = 75, numLeaves = 10)
summary(fastTreeModel)
fastTreeScore <- rxPredict(fastTreeModel, data = mortTest,  
                           extraVarsToWrite = "default")
ftRoc <- rxRoc(actualVarName = "default", predVarNames = "Probability",
               data = fastTreeScore)
rxAuc(ftRoc)
# AUC = 0.91
rxRocCurve(actualVarName = "default", predVarNames = "Probability", 
           data = fastTreeScore)


# rxFastForest
rxFastForestModel <- rxFastForest(binaryFormula, data = mortTrain, 
                                  numTrees = 75, numLeaves = 10)
summary(rxFastForestModel)
FastForestScore   <- rxPredict(rxFastForestModel, data = mortTest,  
                               extraVarsToWrite = "default")
ffRoc <- rxRoc(actualVarName = "default", predVarNames = "Probability",
               data = FastForestScore)
# AUC = 0.76
rxRocCurve(actualVarName = "default", predVarNames = "Probability", 
           data = FastForestScore)


# rxNeuralNet
rxNeuralNetModel <- rxNeuralNet(binaryFormula, data = mortTrain, 
                                numHiddenNodes = 10)
NeuralNetScore   <- rxPredict(rxNeuralNetModel, data = mortTest,  
                              extraVarsToWrite = "default")
# AUC = 0.76
rxRocCurve(actualVarName = "default", predVarNames = "Probability",
           data = NeuralNetScore, title = "ROC Curve for 'default' using NN")


# rxFastLinear
rxFastLinearModel <- rxFastLinear(binaryFormula, data = mortTrain)
summary(rxFastLinearModel)
FastLinearScore   <- rxPredict(rxFastLinearModel, data = mortTest,  
                               extraVarsToWrite = "default")
# AUC =  0.95
rxRocCurve(actualVarName = "default", predVarNames = "Probability",    
           data = FastLinearScore, title = "ROC Curve for 'default' using FL")



# try titanic
tit <- read.csv("Titanic_Rich_Var.csv", stringsAsFactors = FALSE)
tit_excl <- tit[, -c(1, 4, 9, 11, 12)]
tit_df <- as.data.frame(model.matrix(~ . - 1, data = tit_excl))

set.seed(123)
isTest    <- rnorm(nrow(tit_df)) > 0
tit_Train <- tit_df[isTest, ]
tit_Test  <- tit_df[!isTest, ]
binaryFormula <- Survived ~ Pclass + Sexfemale + Age

rxFastLinearModel <- rxFastLinear(binaryFormula, data = tit_Train)
summary(rxFastLinearModel)
FastLinearScore   <- rxPredict(rxFastLinearModel, data = tit_Test,  
                               extraVarsToWrite = "Survived")
# AUC =  0.86
rxRocCurve(actualVarName = "Survived", predVarNames = "Probability.1",    
           data = FastLinearScore, title = "ROC Curve for 'Survived' using FL")


################################################################################
### Multi-class type labels
################################################################################
# trainRows <- c(1:30, 51:80, 101:130)
# testRows  <- !(1:150 %in% trainRows)
# trainIris <- iris[trainRows,]
# testIris  <- iris[testRows,]
multiFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width


# rxLogisticRegression
mlogitModel <- rxLogisticRegression(multiFormula, type = "multiClass", 
                                    data = iris)
summary(mlogitModel)
mlogitScore <- rxPredict(mlogitModel, data = iris, 
                         extraVarsToWrite = "Species")

# predicted Labels
rxCrossTabs(~Species:PredictedLabel, data = mlogitScore, 
            removeZeroCounts = TRUE)


# rxNeuralNet
rxNeuralNetModel <- rxNeuralNet(multiFormula, type = "multiClass",
                                optimizer = sgd(learningRate = 0.2),
                                data = iris)
rxNeuralNetScore <- rxPredict(rxNeuralNetModel, data = iris,  
                              extraVarsToWrite = "Species")

# predicted Labels
rxCrossTabs(~Species:PredictedLabel, data = rxNeuralNetScore, 
            removeZeroCounts = TRUE)



##################################
# Regression type label
##################################
# Sample Data
DF          <- airquality[!is.na(airquality$Ozone), ]
DF$Ozone    <- as.numeric(DF$Ozone)
randomSplit <- rnorm(nrow(DF))
trainAir    <- DF[randomSplit >= 0,]
testAir     <- DF[randomSplit < 0,]

# Regression type label formula
airFormula <- Ozone ~ Solar.R + Wind + Temp



# rxFastTrees
fastTreeModel <- rxFastTrees(airFormula, type = "regression",  data = trainAir)
fastTreeScore <- rxPredict(fastTreeModel, data = testAir,  
                           extraVarsToWrite = "Ozone")
rxLinePlot(Score~Ozone, type = c("smooth", "p"), data = fastTreeScore,
           title = "rxFastTrees", lineColor = "red")


# rxFastForest
rxFastForestModel <- rxFastForest(airFormula, type = "regression",  
                                  data = trainAir)
rxFastForestScore <- rxPredict(rxFastForestModel, data = testAir,  extraVarsToWrite = "Ozone")
rxLinePlot(Score~Ozone, type = c("smooth", "p"), 
           data = rxFastForestScore, title = "rxFastForest", lineColor = "red")


# rxNeuralNet
rxNeuralNetModel <- rxNeuralNet(airFormula, type = "regression",  data = trainAir, numHiddenNodes = 8)
rxNeuralNetScore <- rxPredict(rxNeuralNetModel, data = testAir,  extraVarsToWrite = "Ozone")

rxLinePlot(Score~Ozone, type = c("smooth", "p"), data = rxNeuralNetScore,
           title = "rxNeuralNet", lineColor = "red")


# rxFastLinear with L1Weight and L2Weight
rxFastLinearModel <- rxFastLinear(airFormula, type = "regression",  
                                  data = trainAir, l2Weight = 0.01)
rxFastLinearScore <- rxPredict(rxFastLinearModel, data = testAir, 
                               extraVarsToWrite = "Ozone")
rxLinePlot(Score~Ozone, type = c("smooth", "p"), data = rxFastLinearScore,
           title = "rxFastLinear", lineColor = "red")


