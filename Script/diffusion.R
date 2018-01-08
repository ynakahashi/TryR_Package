################################################################################
##
## Fit various diffusion models
## written by Y.Nakahashi 
## 2018/01/08
##
################################################################################

################################################################################
### environmental settings
################################################################################
install.packages("diffusion")
library(diffusion)

################################################################################
### Run sample script
################################################################################library(mlbench)
### Fit various diffusion models on tsChicken data
plot(tsChicken)

fitbass <- diffusion(tsChicken[, 2], type = "bass")
fitgomp <- diffusion(tsChicken[, 2], type = "gompertz")
fitgsg  <- diffusion(tsChicken[, 2], type = "gsgompertz")

### Plot each model
plot(fitbass)
plot(fitgomp)
plot(fitgsg)
