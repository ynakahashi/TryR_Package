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

library(tidyverse)
library(broom)
library(forcats)
library(ggplot2)


################################################################################
### Run sample script
################################################################################
## Datasaurus
datasaurus <- read_table2("../Data/DatasaurusDozen.tsv",
                          col_names = TRUE, col_types = "cnn") %>%
   mutate(dataset = as_factor(dataset))

print(datasaurus)

ggplot(datasaurus, aes(x = x, y = y)) +
   facet_wrap(~ dataset, nrow = 3) +
   geom_point()


datasaurus %>% 
   group_by(dataset) %>% 
   summarise_each(funs(mean, sd), vars = c(x, y))



## Anscombe
dataAns <- select(anscombe, "x" = x1, "y" = y1) %>% 
   bind_rows(select(anscombe, "x" = x2, "y" = y2)) %>% 
   bind_rows(select(anscombe, "x" = x3, "y" = y3)) %>% 
   bind_rows(select(anscombe, "x" = x4, "y" = y4)) %>% 
   mutate("dataset" = c(rep("I", nrow(anscombe)), rep("II", nrow(anscombe)),
                        rep("III", nrow(anscombe)), rep("IV", nrow(anscombe))))

dataAns %>% 
   group_by(dataset) %>% 
   summarise_each(funs(mean, sd), vars = c(x, y))

dataAns %>% 
   group_by(dataset) %>% 
   summarise("COR" = cor(x, y))

ggplot(dataAns, aes(x = x, y = y, label = dataset)) +
   facet_wrap(~ dataset, nrow = 2) +
   geom_point() +
   geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

dataAns %>% 
   split(.$dataset) %>% 
   map(., ~ lm(y ~ x, data = .) %>% tidy()) %>% 
   bind_rows()


