################################################################################
##
## Apache Spark by Sparklyr.
## written by Y.Nakahashi 
## 2017/5/31
##
################################################################################

################################################################################
## Environmental Settings
################################################################################
## set working directory
work_dir <- "/Users/nakahashi/Desktop/GitTest/TryR_Package/Script"
setwd(work_dir)

# Install Apache Spark via "spark_install" function of "sparklyr".
install.packages("sparklyr")
library(sparklyr)
library(dplyr)
library(readr)
library(ggplot2)
# spark_install(version = "1.6.2")


################################################################################
## Sparklyr update
################################################################################
## Update sparklyr if needed. Also shiny & Rtools will be installed via running below.
devtools::install_github("rstudio/sparklyr")

## Follow the instructions if You faced errors like below:
## 
## 1. Require Visual C++ installation
## Error: Running Spark on Windows requires the Microsoft Visual C++ 2010 SP1 Redistributable Package. Please download and install from: 
## 
##   https://www.microsoft.com/download/en/details.aspx?id=13523
## 
## Restart your rsession after installation completes
## 
## 
## 2. Require JAVA installation
## Error in shell_connection(master = master, spark_home = spark_home, app_name = app_name,  : 
##   Java is required to connect to Spark. Please download and install Java from https://www.java.com/en/
##   
## 
## Restarting Rstudio or Rebooting Windows could be effective when you are suffered from repeated error.


################################################################################
## Run sample script
################################################################################
## Connect to Spark.
sc <- spark_connect(master = "local")

## Load data as spark table.
# install.packages(c("nycflights13", "Lahman"))
iris_tbl    <- copy_to(sc, iris)
flights_tbl <- copy_to(sc, nycflights13::flights, "flights")
batting_tbl <- copy_to(sc, Lahman::Batting, "batting")

src_tbls(sc)


delay <- flights_tbl %>% 
   group_by(tailnum) %>%
   summarise(count = n(), dist = mean(distance), delay = mean(arr_delay)) %>%
   filter(count > 20, dist < 2000, !is.na(delay)) %>%
   collect

## plot delay
ggplot(delay, aes(dist, delay)) +
   geom_point(aes(size = count), alpha = 1/2) +
   geom_smooth() +
   scale_size_area(max_size = 2)



################################################################################
## Data loading
################################################################################
## Load massive data using "sparklyr::spark_read_csv" function
system.time(
   datTmp <- spark_read_csv(sc, "SUSY", 
                            path = "../MicrosoftML/Data/SUSY.csv",
                            header = FALSE)
)

## Load massive data using "readr::read_csv" function
# datTmpDF <- datTmp %>% collect
system.time(
   datTmpDF <- read_csv("../MicrosoftML/Data/SUSY.csv",
                        col_names = FALSE)
)


################################################################################
## Generalised linear model
################################################################################
system.time(
   spark_logit <- datTmp %>%
      ml_logistic_regression(response = "V1",
                             features = c("V2", "V3", "V4", "V5"))
)


cbind(spark_logit$coefficients,
      res_rxGLM$coefficients)
