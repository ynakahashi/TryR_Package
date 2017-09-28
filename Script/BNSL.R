install.packages("BNSL")
library(BNSL)
library(bnlearn)

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
