################################################################################
##
## Try flexmix
## written by Y.Nakahashi 
## 2017/10/3
##
################################################################################

################################################################################
### environmental settings
################################################################################
## Library installation
install.packages("flexmix")
install.packages("githubinstall")
githubinstall::githubinstall('precis')

## Load libraries
library(flexmix)
library(tidyverse)
library(ggplot2)
library(precis)


################################################################################
### Run sample script
################################################################################
## Load data "NPreg"
data("NPreg", package = "flexmix")
dat <- NPreg

## Dummarise data using "precis" package
precis(dat) 
precis(dat, histogram = TRUE) 
summary(dat) 
# There would be differences between values from precis and summary respectively.
# pricis shows hinge values (not quantile) so they match with ones from fivenum.

ggplot(dat, aes(x = yn)) +
   # geom_histogram()
   geom_density()
   
## mixture of two linear regression models. Note that control parameters
## can be specified as named list and abbreviated if unique.
ex1 <- flexmix(yn ~ x + I(x^2), data = dat, k = 2,
               control = list(verb = 5, iter = 100))
summary(ex1)
plot(ex1)
table(ex1@cluster, NPreg$class)

## now we fit a model with one Gaussian response and one Poisson
## response. Note that the formulas inside the call to FLXMRglm are
## relative to the overall model formula.
ex2 <- flexmix(yn ~ x, data = NPreg, k = 2,
               model = list(FLXMRglm(yn ~ . + I(x^2)),
                            FLXMRglm(yp ~ ., family = "poisson")))
plot(ex2)
table(ex2@cluster, NPreg$class)

## for Gaussian responses we get coefficients and standard deviation
parameters(ex2, component = 1, model = 1)

## for Poisson response we get only coefficients
parameters(ex2, component = 1, model = 2)

## fitting a model only to the Poisson response is of course
## done like this
ex3 <- flexmix(yp ~ x, data = NPreg, k = 2,
               model = FLXMRglm(family = "poisson"))
plot(ex3)
ex3
table(ex3@cluster, NPreg$class)

## if observations are grouped, i.e., we have several observations per
## individual, fitting is usually much faster:
ex4 <- flexmix(yp ~ x|id1, data = NPreg, k = 2,
               model = FLXMRglm(family = "poisson"))

## And now a binomial example. Mixtures of binomials are not generically
## identified, here the grouping variable is necessary:
set.seed(1234)
ex5 <- initFlexmix(cbind(yb,1 - yb) ~ x, data = NPreg, k = 2,
                   model = FLXMRglm(family = "binomial"), nrep = 5)
table(NPreg$class, clusters(ex5))
ex6 <- initFlexmix(cbind(yb, 1 - yb) ~ x | id2, data = NPreg, k = 2,
                   model = FLXMRglm(family = "binomial"), nrep = 5)
table(NPreg$class, clusters(ex6))
