################################################################################
##
## Keras.
## written by Y.Nakahashi 
## 2017/6/1
##
################################################################################

################################################################################
### environmental settings
################################################################################
## working directory
wk_Dir <- "C:/Users/Administrator/Desktop/Keras"
setwd(wk_Dir)

## library install & load
devtools::install_github("rstudio/reticulate")
devtools::install_github("rstudio/keras")
library(keras)
library(dplyr)

## install tensorflow
install_tensorflow()

# Error: Installing TensorFlow requires a 64-bit version of Python 3.5
# 
# Please install 64-bit Python 3.5 to continue, supported versions include:
#    
#    - Anaconda Python (Recommended): https://www.continuum.io/downloads#windows
# - Python Software Foundation   : https://www.python.org/downloads/release/python-353/
#    
#    Note that if you install from Python Software Foundation you must install exactly
# Python 3.5 (as opposed to 3.6 or higher).

## install tensorflow
install_tensorflow()


################################################################################
### Run sample script for Sequential model
################################################################################
## prepare sample data
set.seed(123)
learn_id <- sample(1:nrow(iris), 100, replace = FALSE)

train_data <- iris %>%
   filter(row.names(.) %in% learn_id)
test_data  <- iris %>%
   filter(!row.names(.) %in% learn_id)

## training data
x_train <- train_data %>% 
   select(-Species) %>%
   as.matrix()
y_train <- as.matrix(model.matrix(~ train_data$Species - 1))


## test data
x_test <- test_data %>% 
   select(-Species) %>%
   as.matrix()
y_test <- as.matrix(model.matrix(~ test_data$Species - 1))


## generate model object
model <- keras_model_sequential() 

# > Model
# Model
# _______________________________________________________________________________
# Layer (type)                       Output Shape                    Param #     
# ===============================================================================
#    Total params: 0
# Trainable params: 0
# Non-trainable params: 0
# _______________________________________________________________________________


model %>% 
   layer_dense(units = 10, input_shape = 4) %>% # Output & Input dimension
   layer_activation(activation = 'relu') %>% 
   layer_dense(units = 3) %>% 
   layer_activation(activation = 'softmax')

# > model
# Model
# _______________________________________________________________________________
# Layer (type)                       Output Shape                    Param #     
# ===============================================================================
# dense_3 (Dense)                    (None, 10)                      50          
# _______________________________________________________________________________
# activation_3 (Activation)          (None, 10)                      0           
# _______________________________________________________________________________
# dense_4 (Dense)                    (None, 3)                       33          
# _______________________________________________________________________________
# activation_4 (Activation)          (None, 3)                       0           
# ===============================================================================
# Total params: 83.0
# Trainable params: 83.0
# Non-trainable params: 0.0
# _______________________________________________________________________________


## model parameter
model %>% 
   compile(
      loss = 'categorical_crossentropy',
      optimizer = optimizer_sgd(lr = 0.02),
      metrics = c('accuracy'))

## fitting
model %>% 
   fit(x_train, y_train, epochs = 1000, batch_size = 32)

# Epoch 990/1000
# 100/100 [==============================] - 0s - loss: 0.0432 - acc: 0.9800     
# Epoch 991/1000
# 100/100 [==============================] - 0s - loss: 0.0442 - acc: 0.9800     
# Epoch 992/1000
# 100/100 [==============================] - 0s - loss: 0.0455 - acc: 0.9800     
# Epoch 993/1000
# 100/100 [==============================] - 0s - loss: 0.0508 - acc: 0.9800     
# Epoch 994/1000
# 100/100 [==============================] - 0s - loss: 0.0602 - acc: 0.9800     
# Epoch 995/1000
# 100/100 [==============================] - 0s - loss: 0.0437 - acc: 0.9800     
# Epoch 996/1000
# 100/100 [==============================] - 0s - loss: 0.0433 - acc: 0.9800     
# Epoch 997/1000
# 100/100 [==============================] - 0s - loss: 0.0432 - acc: 0.9800     
# Epoch 998/1000
# 100/100 [==============================] - 0s - loss: 0.1129 - acc: 0.9600     
# Epoch 999/1000
# 100/100 [==============================] - 0s - loss: 0.0529 - acc: 0.9700     
# Epoch 1000/1000
# 100/100 [==============================] - 0s - loss: 0.0647 - acc: 0.9600 

## test
loss_and_metrics <- model %>% 
   evaluate(x_test, y_test, batch_size = 128)

# > loss_and_metrics
# [[1]]
# [1] 0.1853937
# 
# [[2]]
# [1] 0.96

## predict
classes <- model %>% 
   predict(x_test, batch_size = 128)

# > classes
# [,1]         [,2]         [,3]
# [1,] 9.983064e-01 1.693639e-03 6.461022e-20
# [2,] 9.971897e-01 2.810343e-03 7.951551e-19
# [3,] 9.983315e-01 1.668578e-03 3.816257e-20
# [4,] 9.984826e-01 1.517428e-03 4.521292e-20
# [5,] 9.994913e-01 5.087039e-04 3.988174e-23

