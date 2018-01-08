devtools::install_github("rstudio/tensorflow")
library(tensorflow)
library(dplyr)
library(caret)
library(glmnet)
library(ranger)



tf$VERSION

## hello, tensorflow
sess  <- tf$Session()
hello <- tf$constant("Hello, TensorFlow!")
sess$run(hello)

## single regresion
set.seed(123)
x_data <- runif(100, min=0, max=1)
y_data <- x_data * 1.5 - 12

W <- tf$Variable(tf$random_uniform(shape(1L), -1.0, 1.0))
# W <- tf$Variable(tf$zeros(shape(1L)))
b <- tf$Variable(tf$zeros(shape(1L)))
y <- W * x_data + b

loss  <- tf$reduce_mean((y-y_data)^2)
opt   <- tf$train$GradientDescentOptimizer(0.2)
train <- opt$minimize(loss)

sess <- tf$Session()
# sess$run(tf$global_variables_initializer())
sess$run(tf$initialize_all_variables())

for (step in 1:1001){
  sess$run(train)
  if (step %% 20 == 0) {
    cat(step, "-", sess$run(W), sess$run(b), "\n")
  }
}



## iris, multinomial classification
W  <- tf$Variable(tf$zeros(shape(4L, 3L)))
x  <- tf$placeholder(tf$float32, shape(NULL, 4L))
b  <- tf$Variable(tf$zeros(3L))
y  <- tf$nn$softmax(tf$matmul(x, W)+b)
y_ <- tf$placeholder(tf$float32, shape(NULL, 3L))

loss  <- tf$reduce_mean(-tf$reduce_sum(y_*y, reduction_indices = 1L))
opt   <- tf$train$GradientDescentOptimizer(0.9)
train <- opt$minimize(loss)

trn_x <- iris %>% 
  select(-Species) %>% 
  as.matrix()
trn_y <- iris %>% 
  dummyVars(formula = ~ Species, sep = NULL) %>% 
  predict(object = ., newdata = iris) %>% 
  as.matrix()

sess <- tf$Session()
sess$run(tf$initialize_all_variables())

N    <- 20000
each <- 1000
resAll <- matrix(NA, N, 15)
for (i in 1:N){
  sess$run(train, dict(x=trn_x, y_=trn_y))
  resAll[i, ] <- c(sess$run(W), sess$run(b))
  if (i %% each == 0) {
    cat(i, "-", sess$run(W), sess$run(b), "\n")
    print(table(
      predict = sess$run(fetches = y, feed_dict = dict(x = trn_x, y_ = trn_y)) %>%
        apply(MARGIN = 1, FUN = which.max),
      true = trn_y %>% 
        apply(MARGIN = 1, FUN = which.max)))
  }
}

## compare with glmnet
resGLM <- glmnet(trn_x, iris$Species, "multinomial")
glmnet_confusion_mat <- table(
      predict = predict(resglm, newx = trn_x, type = "class", s = 0.01)[, 1],
      true = iris$Species
    )


## compare with random forest
resRF <- ranger(Species ~., data=iris, mtry=2, num.trees = 500, write.forest=TRUE)
table(iris$Species, predict(resRF, data=iris)$predictions)

