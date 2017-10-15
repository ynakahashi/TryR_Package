install.packages("BNSL")
library(BNSL)
library(bnlearn)
library(mlbench)

res <- bnsl(asia)

set.seed(123)
X <- rnorm(100, 0, 2)
Y <- X * 0.5 + rnorm(100, 0, 4)
Z <- Y * 0.8 + rnorm(100, 0, 2)
tmp <- data.frame(
   "X" = X,
   "Y" = Y,
   "Z" = Z
)

res <- bnsl(tmp)
plot(res)




set.seed(1)
n     <- 100
p     <- 40
sigma <- 1

sim <- mlbench.friedman1(n, sd = sigma)
colnames(sim$x) <- c(paste("real", 1:5, sep = ""),
                     paste("bogus", 1:5, sep = ""))
# bogus <- matrix(rnorm(n * p), nrow = n)
# colnames(bogus) <- paste("bogus", 5 + (1:ncol(bogus)), sep = "")
# x <- cbind(sim$x, bogus)
dat <- data.frame(
   cbind(y = sim$y, sim$x))

res <- bnsl(dat)
plot(res)


