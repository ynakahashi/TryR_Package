################################################################################
##
## Anscobme's quartet.
## written by Y.Nakahashi 
## 2017/7/4
##
################################################################################

################################################################################
### environmental settings
################################################################################
## working directory
work_dir <- "/Users/nakahashi/Desktop/GitTest/TryR_Package/Script"
setwd(work_dir)

## library install & load
# install.packages("tweenr")
# library(tweenr)
# devtools::install_github("dgrtwo/gganimate")
# library(gganimate)
library(tidyverse)
library(forcats)
library(ggplot2)


################################################################################
### Run sample script
################################################################################
datasaurus <- read_table2("../Data/SameStatsDataAndImages/datasets/DatasaurusDozen.tsv",
                          col_names = TRUE, col_types = "cnn") %>%
   mutate(dataset = as_factor(dataset))

print(datasaurus)

ggplot(datasaurus, aes(x = x, y = y)) +
   facet_wrap(~ dataset, nrow = 3) +
   geom_point()


# p <- ggplot(datasaurus, aes(x = x, y = y)) +
#    geom_point(aes(frame = dataset))
# 
# animation::ani.options(interval = 1)
# gganimate(p, title_frame = FALSE)



