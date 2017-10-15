################################################################################
##
## Variable selection by genetic algorithm via caret package
## written by Y.Nakahashi 
## 2017/10/15
##
################################################################################

################################################################################
### environmental settings
################################################################################
library(caret)
library(mlbench)
library(desirability)

################################################################################
### Run sample script
################################################################################library(mlbench)
## Generate simulation data
set.seed(1)
n     <- 100
p     <- 40
sigma <- 1

sim <- mlbench.friedman1(n, sd = sigma)
colnames(sim$x) <- c(paste("real", 1:5, sep = ""),
                     paste("bogus", 1:5, sep = ""))
bogus <- matrix(rnorm(n * p), nrow = n)
colnames(bogus) <- paste("bogus", 5 + (1:ncol(bogus)), sep = "")
x <- cbind(sim$x, bogus)
y <- sim$y

## Normalization
normalization <- preProcess(x)
x <- predict(normalization, x)
x <- as.data.frame(x)

## 
ga_ctrl <- gafsControl(functions = rfGA,
                       method = "repeatedcv",
                       repeats = 5)

set.seed(10)
rf_ga <- gafs(x = x, y = y,
              iters = 200,
              gafsControl = ga_ctrl)
rf_ga

plot(rf_ga) + theme_bw()



rfGA2 <- rfGA
rfGA2$fitness_intern <- function (object, x, y, maximize, p) {
   RMSE <- rfStats(object)[1]
   d_RMSE <- dMin(0, 4)
   d_Size <- dMin(1, p, 2)
   overall <- dOverall(d_RMSE, d_Size)
   D <- predict(overall, data.frame(RMSE, ncol(x)))
   c(D = D, RMSE = as.vector(RMSE))
}

ga_ctrl_d <- gafsControl(functions = rfGA2,
                         method = "repeatedcv",
                         repeats = 5,
                         metric = c(internal = "D", external = "RMSE"),
                         maximize = c(internal = TRUE, external = FALSE))

set.seed(10)
rf_ga_d <- gafs(x = x, y = y,
                iters = 200,
                gafsControl = ga_ctrl_d)

rf_ga_d


