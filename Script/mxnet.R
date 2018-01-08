##
##
##

# install.packages("drat", repos="https://cran.rstudio.com")
# drat:::addRepo("dmlc")
# install.packages("mxnet")
# install.packages("dplyr")
# install.packages("tidyr")


library(mxnet)
library(dplyr)
library(tidyr)

dat <- quakes
nl <- nrow(dat) * 0.9
dat$Label <- runif(nrow(dat))

dat %>% filter(Label <= 0.9) %>% select(-Label) %>% data.matrix() -> datLrn
dat %>% filter(Label >  0.9) %>% select(-Label) %>% data.matrix() -> datPrd

xLrn <- datLrn[, -5]
yLrn <- datLrn[, 5]
xPrd <- datPrd[, -5]
yPrd <- datPrd[, 5]

data <- mx.symbol.Variable("data")
fc1  <- mx.symbol.FullyConnected(data, num_hidden=1)
lro  <- mx.symbol.LinearRegressionOutput(fc1)
mx.set.seed(0)
model <- mx.model.FeedForward.create(lro, X=xLrn, y=yLrn,
                                     eval.data=list(data=xPrd, label=yPrd),
                                     ctx=mx.cpu(), num.round=10, array.batch.size=20,
                                     learning.rate=2e-6, momentum=0.9, 
                                     eval.metric=mx.metric.rmse)






train <- read.csv('https://github.com/ozt-ca/tjo.hatenablog.samples/raw/master/r_samples/public_lib/jp/mnist_reproduced/short_prac_train.csv')
test <- read.csv('https://github.com/ozt-ca/tjo.hatenablog.samples/raw/master/r_samples/public_lib/jp/mnist_reproduced/short_prac_test.csv')

train <- data.matrix(train)
test  <- data.matrix(test)

train.x <- train[, -1]
train.y <- train[, 1]

train.x <- train.x/255

test_org <- test
test <- test[, -1]
test <- t(test/255)


data <- mx.symbol.Variable("data")
fc1 <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=128)
act1 <- mx.symbol.Activation(fc1, name="relu1", act_type="relu")
fc2 <- mx.symbol.FullyConnected(data, name="fc2", num_hidden=64)
act2 <- mx.symbol.Activation(fc2, name="relu2", act_type="relu")
fc3 <- mx.symbol.FullyConnected(data, name="fc3", num_hidden=10)
softmax <- mx.symbol.SoftmaxOutput(fc3, name="sm")
devices <- mx.cpu()
mx.set.seed(0)
model <- mx.model.FeedForward.create(softmax, X=train.x, y=train.y, ctx=devices,
                                     num.round=10, array.batch.size=100,
                                     learning.rate=0.07, momentum=0.9,
                                     eval.metric=mx.metric.accuracy,
                                     initializer=mx.init.uniform(0.07),
                                     epoch.end.callback=mx.callback.log.train.metric(100))




data(BostonHousing, package="mlbench")
train.ind <- seq(1, 506, 3)
train.x   <- data.matrix(BostonHousing[train.ind, -14])
test.x    <- data.matrix(BostonHousing[-train.ind, -14])
test.y    <- BostonHousing[-train.ind, 14]
data      <- mx.symbol.Variable("data")
fc1       <- mx.symbol.FullyConnected(data, num_hidden=1)
lro       <- mx.symbol.LinearRegressionOutput(fc1)
mx.set.seed(0)
model <- mx.model.FeedForward.create(lro, X=train.x, y=train.y,
                                     eval.data=list(data=test.x, label=test.y),
                                     ctx=mx.cpu(), num.round=10, array.batch.size=20,
                                     learning.rate=2e-6, momentum=0.9, 
                                     eval.metric=mx.metric.rmse)

train.y   <- BostonHousing[train.ind, 14]
cor(cbind(test.y, t(predict(model, test.x))))

