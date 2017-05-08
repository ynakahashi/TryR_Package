################################################################################
##
## Build Bayesian Network Model by bnlearn.
## written by Y.Nakahashi 
## 2017/4/6
##
################################################################################

################################################################################
## Environmental Settings
################################################################################
## set working directory
# work_dir <- "/Users/nakahashi/Desktop/GitTest/TryRstan"
work_dir <- "/Users/ynakahashi/Desktop/ynakahashi_git/TryBN"
setwd(work_dir)

## install & load bnlearn
install.packages("bnlearn")
library(bnlearn)

################################################################################
## run sample script
################################################################################
## create sample data
set.seed(123)
norm     <- rnorm(4000)
dat_smpl <- data.frame(matrix(norm, nrow=1000, ncol=4, byrow=T))
colnames(dat_smpl) <- c("Height", "BMI", "SBP", "FBS")

dat_ana <- dat_smpl
dat_ana$Height <- 170 + dat_smpl$Height * 10
dat_ana$SBP    <- 120 + dat_smpl$SBP * 10
dat_ana$BMI    <- 22 + dat_smpl$Height * 2 + dat_smpl$BMI * 2 + dat_smpl$SBP * 2
dat_ana$FBS    <- 90 + (dat_smpl$BMI - 22) * 5 + dat_smpl$FBS * 10


## run Bayesian Network
res_BN01 <- gs(dat_ana)
plot(res_BN01)

res_BN02 <- set.arc(res_BN01, "BMI", "FBS")
arc.strength(res_BN02, dat_ana)
plot(res_BN02)

fitted <- bn.fit(res_BN02, dat_ana)
coef <- coefficients(fitted)
print(coef)