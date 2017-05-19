################################################################################
##
## Time-series forecast by Prophet.
## written by Y.Nakahashi 
## 2017/5/19
##
################################################################################

################################################################################
## Environmental Settings
################################################################################
## set working directory
work_dir <- "/Users/nakahashi/Desktop/GitTest/TryR_Package/Script"
setwd(work_dir)

## install & load prophet
# install.packages("prophet")
library(prophet)
library(dplyr)

################################################################################
## Run sample script
################################################################################
df <- read.csv("../Data/example_wp_peyton_manning.csv") %>%
   mutate(y = log(y))
res <- prophet(df)

future   <- make_future_dataframe(res, periods = 365)
forecast <- predict(res, future)

plot(res, forecast)
prophet_plot_components(res, forecast)


################################################################################
## Compare with STL
################################################################################
## simulate sample data
set.seed(123)
num_year <- 5
n        <- 365 * num_year
trend    <- sin(seq(1, 2, length = n)) / 5
week     <- rep(cos(seq(1, -1, length = 7)) / 10, length.out = n)
day      <- rep(atan(seq(0, 5, length = 365)) / 100, num_year)
error    <- rnorm(n, 0, 0.01)
y        <- trend + week + day + error
plot(y, type = "l")

tmp_data <- data.frame("ds" = as.Date("2009-12-31") + 1:n,
                       "y" = y)

## run STL
res_STL <- stl(ts(tmp_data$y, frequency = 365),
               s.window = "periodic")$time.series %>% data.frame()

## run Prophet
tmp_data$ds <- as.factor(tmp_data$ds)
res_Prp     <- prophet(tmp_data)
future      <- make_future_dataframe(res_Prp, periods = 1)
forecast    <- predict(res_Prp, future)
plot(res_Prp, forecast)
prophet_plot_components(res_Prp, forecast)

plot(zoo::as.zoo(as.ts(
   cbind(trend, res_STL$trend, forecast$trend[1:(length(forecast$trend)-1)]))),
   plot.type = "single", col=1:3)

plot(zoo::as.zoo(as.ts(
   cbind(day, res_STL$seasonal[1:365], forecast$yearly[1:365]))),
   plot.type = "single", col=1:3)




