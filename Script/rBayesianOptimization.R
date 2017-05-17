################################################################################
##
## Optimization by rBayesianOptimization.
## written by Y.Nakahashi 
## 2017/5/17
##
################################################################################

################################################################################
## Environmental Settings
################################################################################
## set working directory
work_dir <- "/Users/nakahashi/Desktop/GitTest/TryR_Package/Script"
setwd(work_dir)

## install & load bnlearn
install.packages("rBayesianOptimization")
library(rBayesianOptimization)

################################################################################
## simple simulation
################################################################################
set.seed(123)
simulate_y_01 <- function(pars) {
   X1 <- rnorm(100, 100, 2) * rep(c(0, rep(1, 7), 0, 1), 10)
   X2 <- filter(X1, pars[1], "recursive")
   Z1 <- rnorm(100, 5, 2) * rep(c(rep(0, 5), rep(1, 5)), 10)
   dat <- data.frame(
      "Y"  = 50 + pars[2] * X2 + pars[3] * Z1 + rnorm(100, 0, pars[4]),
      "X1" = X1,
      "Z1" = Z1)
   return(dat)
}

return_AIC_01 <- function(a) {
   dat$Tmp <- filter(dat$X1, a, "recursive")
   AIC(lm(Y ~ Tmp + Z1, dat))
}

## low lambda
pars      <- c(0.1, 0.5, 4, 5.0)
n         <- 500
res_01_01 <- matrix(0, n, 4)
for (i in 1:n) {
   dat <- simulate_y_01(pars)
   a   <- optimise(return_AIC_01, c(0, 1))$minimum
   X2  <- filter(dat$X1, a, "recursive")
   res_01_01[i, ] <- c(a, coef(lm(Y ~ X2 + Z1, data = dat)))
}
summary(res_01_01)
MASS::truehist(res_01_01[, 1])


## High lambda
pars      <- c(0.9, 0.5, 4, 5.0)
n         <- 500
res_01_02 <- matrix(0, n, 4)
for (i in 1:n) {
   dat <- simulate_y_01(pars)
   a   <- optimise(return_AIC_01, c(0, 1))$minimum
   X2  <- filter(dat$X1, a, "recursive")
   res_01_02[i, ] <- c(a, coef(lm(Y ~ X2 + Z1, data = dat)))
}
summary(res_01_02)
MASS::truehist(res_01_02[, 1])


################################################################################
## 2 variables
################################################################################
simulate_y_02 <- function(pars) {
   X1 <- rnorm(100, 100, 2) * rep(c(0, rep(1, 7), 0, 1), 10)
   X2 <- filter(X1, pars[1], "recursive")
   Z1 <- rnorm(100, 5, 2) * rep(c(rep(0, 5), rep(1, 5)), 10)
   Z2 <- filter(Z1, pars[2], "recursive")
   dat <- data.frame(
      "Y"  = 50 + pars[3] * X2 + pars[4] * Z2 + rnorm(100, 0, pars[5]),
      "X1" = X1,
      "Z1" = Z1)
   return(dat)
}

return_AIC_02 <- function(param) {
   a <- param[1]
   b <- param[2]
   dat$TmpX <- filter(dat$X1, a, "recursive")
   dat$TmpZ <- filter(dat$Z1, b, "recursive")
   AIC(lm(Y ~ TmpX + TmpZ, dat))
}

init_par <- array(c(0.5, 0.5))
optim(par = optim(par = init_par, fn = return_AIC)$par, 
      fn = return_AIC)$par

## Low lambda
pars      <- c(0.1, 0.1, 0.5, 4, 5.0)
n         <- 100
res_02_01 <- matrix(0, n, 5)
for (i in 1:n) {
   dat <- simulate_y(pars)
   res <- optim(par = optim(par = init_par, fn = return_AIC)$par, 
                fn = return_AIC)$par
   X2  <- filter(dat$X1, res[1], "recursive")
   Z2  <- filter(dat$Z1, res[2], "recursive")
   res_02_01[i, ] <- c(res, coef(lm(Y ~ X2 + Z2, data = dat)))
}
summary(res_02_01)


## High lambda
pars      <- c(0.8, 0.7, 0.5, 4, 5.0)
n         <- 100
res_02_02 <- matrix(0, n, 5)
for (i in 1:n) {
   dat <- simulate_y(pars)
   res <- optim(par = optim(par = init_par, fn = return_AIC)$par, 
                fn = return_AIC)$par
   X2  <- filter(dat$X1, res[1], "recursive")
   Z2  <- filter(dat$Z1, res[2], "recursive")
   res_02_02[i, ] <- c(res, coef(lm(Y ~ X2 + Z2, data = dat)))
}
summary(res_02_02)


################################################################################
## Bayesian Optimization
################################################################################

return_AIC_BO <- function(a, b) {
   dat$TmpX <- filter(dat$X1, a, "recursive")
   dat$TmpZ <- filter(dat$Z1, b, "recursive")
   out <- list(Score = -AIC(lm(Y ~ TmpX + TmpZ, dat)),
               Pred  = -AIC(lm(Y ~ TmpX + TmpZ, dat)))
   return(out)
}

pars   <- c(0.1, 0.1, 0.5, 4, 5.0)
dat    <- simulate_y(pars)
res_BO <- BayesianOptimization(return_AIC_BO, bounds = list(a = c(0, 1), b = c(0, 1)),
                               init_points = 20, n_iter = 1, acq = "ucb",
                               kappa = 2.576, eps = 0, verbose = TRUE)

init_par <- array(c(0.5, 0.5))
optim(par = optim(par = init_par, fn = return_AIC)$par, 
      fn = return_AIC)$par


pars   <- c(0.8, 0.8, 0.5, 4, 5.0)
dat    <- simulate_y(pars)
res_BO <- BayesianOptimization(return_AIC_BO, bounds = list(a = c(0, 1), b = c(0, 1)),
                               init_points = 100, n_iter = 2, acq = "ucb",
                               kappa = 2.576, eps = 0, verbose = TRUE)

init_par <- array(c(0.5, 0.5))
optim(par = optim(par = init_par, fn = return_AIC)$par, 
      fn = return_AIC)$par
