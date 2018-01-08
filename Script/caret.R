###############
## Machine Learning using "caret" package
##    written by Y.Nakahashi 2015-10-08
###############

##### library setting
library(caret)
library(plyr)
library(dplyr)
library(C50)
library(randomForest)
library(kernlab)
library(e1071)
library(pROC)
library(doParallel)
library(caretEnsemble)
library(GGally)
library(gbm)

##### set sample data "churnTrain" & "churnTest"
data(churn)
churnTrain %>% head
churnTest  %>% head

churnTrain %>% dim
churnTest  %>% dim


##### predictive model using Random Forest(RF)
## set random seed
set.seed(123)

## set training parameter. cv means Cross Validation.
trControl <- trainControl(method="cv", number=10, classProbs=TRUE,
                          summaryFunction=twoClassSummary)

## train the model.
system.time(
   fitRF <- train(churn~., data=churnTrain, method="rf", metric="ROC",
                  tuneGrid=data.frame(.mtry=c(5, 10)), trControl=trControl)
)

## print best hyper-paramter(in case of RF, number of variables usable at a try)
fitRF$bestTune


##### validation based on ROC(or AUC)
## prediction
pred <- predict(fitRF, churnTest)
prob <- predict(fitRF, churnTest, type="prob")

## ROC curve
rocCurve <- roc(response=pred, predictor=prob$yes, levels=rev(levels(pred)))
plot(rocCurve, legacy.axes=TRUE)

## AUC (in this data, AUC is 1.0 because all samples are separated perfectly)
auc(rocCurve)

## confusin matrix
confusionMatrix(data=pred, reference=churnTest$churn, positive="yes")

## final model(trained using all data, best hyper-parameter)
fitRF$finalModel




##### predictive model using Support Vector Machine(SVM)
## set random seed
set.seed(123)

## train the model under same training environment
system.time(
   fitSVM <- train(churn~., data=churnTrain, method="svmRadial", metric="ROC",
                   trControl=trControl)
)

## compare model accuracy
resamp    <- resamples(list(svm=fitSVM, rf=fitRF))
modelDiff <- diff(resamp)
summary(resamp)
summary(modelDiff)



##### speeding-up by parallel computing 
trControlPar <- trainControl(method="cv", number=10, 
                             summaryFunction=twoClassSummary, classProbs=TRUE)

## make clusters
cls <- makeCluster(detectCores())

## allow parallel computing
trControlPar$allowParallel <- TRUE
registerDoParallel(cls)
system.time(
   fitRFPar <- train(churn~., data=churnTrain, method="rf", metric="ROC",
                     tuneGrid=data.frame(.mtry=c(5, 10)), 
                     trControl=trControlPar)
)

## pause parallel computing
registerDoSEQ()
trControlPar$allowParallel <- FALSE
system.time(
   fitRFNonPar <- train(churn~., data=churnTrain, method="rf", metric="ROC",
                        tuneGrid=data.frame(.mtry=c(5, 10)),
                        trControl=trControlPar)
)




##### ensemble learning
## set random seed
set.seed(123)

## set no. of fold & no. of repeat
folds   <- 10
repeats <- 1

## training parameter
trControlEn <- trainControl(method="cv", number=folds, classProbs=TRUE,
                            savePredictions=TRUE, summaryFunction=twoClassSummary,
                            index=createMultiFolds(churnTrain$churn, k=folds,
                                                   times=repeats))

## predictive model using SVM, RF, Gradient Boosting(from GBM package)
modelList <- caretList(churn~., data=churnTrain, metric="ROC",
                       trControl=trControlEn, verbose=FALSE,
                       methodList=c("svmRadial", "rf", "gbm"))


## draw graph for each model
predEach <- (1-predict(modelList, churnTest)) %>% as.data.frame %>% 
   mutate(churn=churnTest$churn)
ggpairs(predEach, colour="churn", shape="churn", pointsize=6)

## create meta-model(ensemble learning)
glmStack  <- caretStack(modelList, method="glm", metric="ROC",
                        trControl=trControlEn)
predStack <- (1-predict(modelList, churnTest)) %>% as.data.frame %>%
   mutate(stacking=1-predict(glmStack, churnTest, type="prob")$yes, 
          churn=churnTest$churn)
ggpairs(predStack, colour="churn", shape="churn", pointsize=6)


## draw ROC curve for each model
response <- predStack$churn
lvs      <- rev(levels(predStack$churn))
rocSVM   <- roc(response=response, predictor=predStack$svmRadial, levels=lvs)
rocRF    <- roc(response=response, predictor=predStack$rf,        levels=lvs)
rocGBM   <- roc(response=response, predictor=predStack$gbm,       levels=lvs)
rocStack <- roc(response=response, predictor=predStack$stacking,  levels=lvs)

plot(rocSVM,  lty="dashed",  legacy.axes=TRUE)
lines(rocRF,  lty="dotted",  col="green")
lines(rocGBM, lty="dotdash", col="blue")
lines(rocStack, col="red")
legend("bottomright",
       legend=c("SVM", "RF", "GBM", "Stack"),
       col=c("black", "green", "blue", "red"), 
       lty=c("dashed", "dotted", "dotdash", "solid"))

## AUC
auc(rocSVM)
auc(rocRF)
auc(rocGBM)
auc(rocStack)
