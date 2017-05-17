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
## run sample script
################################################################################
set.seed(123)
simulate_y <- function(pars) {
   X1 <- rnorm(100, 100, 2) * rep(c(0, rep(1, 7), 0, 1), 10)
   X2 <- filter(X1, pars[1], "recursive")
   Z1 <- rnorm(100, 5, 2) * rep(c(rep(0, 5), rep(1, 5)), 10)
   dat <- data.frame(
      "Y" = 50 + pars[2] * X2 + pars[3] * Z1 + rnorm(100, 0, pars[4]),
      "X1" = X1,
      "Z1" = Z1)
   return(dat)
}

return_AIC <- function(a) {
   # dat     <- simulate_y(pars)
   dat$Tmp <- filter(dat$X1, a, "recursive")
   AIC(lm(Y ~ Tmp + Z1, dat))
}

pars <- c(0.2, 0.5, 4, 0.3)
dat  <- simulate_y(pars)
optimise(return_AIC, c(0, 1))$minimum


n     <- 500
res_a <- matrix(0, n, 2)
for (i in 1:n) {
   dat <- simulate_y(pars)
   a   <- optimise(return_AIC, c(0, 1))$minimum
   X2  <- filter(dat$X1, a, "recursive")
   res_a[i, ] <- c(a, coef(lm(Y ~ X2 + Z1, data = dat))[2])
}
summary(res_a)
MASS::truehist(res_a[, 1])
MASS::truehist(res_a[, 2])
