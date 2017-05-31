---
title: "Try Apache Spark in R"
output: html_notebook
---

Install Apache Spark via "spark_install" function of "sparklyr".
```{r}
# install.packages("sparklyr")
library(sparklyr)
library(dplyr)
library(readr)
library(MicrosoftML)
library(RevoScaleR)
library(ggplot2)
# spark_install(version = "1.6.2")
```

Update sparklyr if needed. Also shiny & Rtools will be installed via running below chunk.
```{r}
# devtools::install_github("rstudio/sparklyr")
```
Follow the instructions if You faced errors like below:

1. Require Visual C++ installation
Error: Running Spark on Windows requires the Microsoft Visual C++ 2010 SP1 Redistributable Package. Please download and install from: 

  https://www.microsoft.com/download/en/details.aspx?id=13523

Restart your rsession after installation completes


2. Require JAVA installation
Error in shell_connection(master = master, spark_home = spark_home, app_name = app_name,  : 
  Java is required to connect to Spark. Please download and install Java from https://www.java.com/en/
  

Restarting Rstudio or Rebooting Windows could be effective when you are suffered from repeated error.


Connect to Spark.

```{r}
sc <- spark_connect(master = "local")
```

Load data as spark table.

```{r}
# install.packages(c("nycflights13", "Lahman"))
iris_tbl    <- copy_to(sc, iris)
flights_tbl <- copy_to(sc, nycflights13::flights, "flights")
batting_tbl <- copy_to(sc, Lahman::Batting, "batting")
```

```{r}
src_tbls(sc)
```


```{r}
delay <- flights_tbl %>% 
   group_by(tailnum) %>%
   summarise(count = n(), dist = mean(distance), delay = mean(arr_delay)) %>%
   filter(count > 20, dist < 2000, !is.na(delay)) %>%
   collect
```

plot delay
```{r}
ggplot(delay, aes(dist, delay)) +
   geom_point(aes(size = count), alpha = 1/2) +
   geom_smooth() +
   scale_size_area(max_size = 2)
```



Load massive data using "sparklyr::spark_read_csv" function

```{r}
system.time(
   datTmp <- spark_read_csv(sc, "SUSY", 
                            path = "../MicrosoftML/Data/SUSY.csv",
                            header = FALSE)
)
```

Load massive data using "readr::read_csv" function

```{r}
# datTmpDF <- datTmp %>% collect
system.time(
   datTmpDF <- read_csv("../MicrosoftML/Data/SUSY.csv",
                        col_names = FALSE)
)
```


```{r}
system.time(
   spark_logit <- datTmp %>%
      ml_logistic_regression(response = "V1",
                             features = c("V2", "V3", "V4", "V5"))
)
```


```{r}
system.time(
   res_rxGLM  <- rxGlm(X1 ~ X2 + X3 + X4 + X5, 
                       family = binomial("logit"), 
                       data = datTmpDF)
)
```


```{r}
cbind(spark_logit$coefficients,
      res_rxGLM$coefficients)
```
