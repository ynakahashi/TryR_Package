################################################################################
##
## Try Bayesian-Optimization
## https://github.com/ymattu/MlBayesOpt
## 2017/5/8
##
################################################################################

################################################################################
## Environmental Settings
################################################################################
## set working directory
work_dir <- "/Users/nakahashi/Desktop/GitTest/TryRstan"
# work_dir <- "/Users/ynakahashi/Desktop/ynakahashi_git/TryBN"
setwd(work_dir)

## install & load MlBayesOpt
# devtools::install_github("ymattu/MlBayesOpt")
library(MlBayesOpt)


################################################################################
## run sample script
################################################################################
## SVM
set.seed(123)
res <- svm_opt(
   train_data = iris_train,
   train_label = iris_train$Species,
   test_data = iris_test,
   test_label = iris_test$Species,
   acq = "ucb"
)

## Random forest
## isn't work...
set.seed(123)
res <- rf_opt(
   train_data = iris_train,
   train_label = iris_train$Species,
   test_data = iris_test,
   test_label = iris_test$Species,
   mtry_range = c(1L, 4L)
)

## XGboost
set.seed(123)
res <- xgb_opt(
   train_data = iris_train,
   train_label = iris_train$Species,
   test_data = iris_test,
   test_label = iris_test$Species,
   objectfun = "multi:softmax",
   classes = 3,
   evalmetric = "merror"
)
