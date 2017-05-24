################################################################################
##
## Try plotly.
## written by Y.Nakahashi 
## 2017/5/24
##
################################################################################

################################################################################
## Environmental Settings
################################################################################
## set working directory
work_dir <- "/Users/nakahashi/Desktop/GitTest/TryR_Package/Script"
setwd(work_dir)

## install & load prophet
# install.packages("plotly")
library(plotly)

################################################################################
## Run sample script
################################################################################
# histogram
plot_ly(economics, x = ~ pop)
plot_ly(economics, x = ~ date, y = ~ pop)
plot_ly(z = ~ volcano)
plot_ly(z = ~ volcano, type = "surface")

add_lines(plot_ly(economics, x = ~ date, y = ~ unemploy/pop))

economics %>% plot_ly(x = ~ date, y = ~ unemploy/pop) %>% add_lines()

plot_ly(economics, x = ~ date, color = I("black")) %>%
   add_lines(y = ~ uempmed) %>%
   add_lines(y = ~ psavert, color = I("red"))

p <- plot_ly(iris, x = ~ Sepal.Width, y = ~ Sepal.Length) 
add_markers(p, color = ~ Petal.Length, size = ~ Petal.Length)
add_markers(p, color = ~ Species)
add_markers(p, color = ~ Species, colors = "Set1")
add_markers(p, symbol = ~ Species)
add_paths(p, linetype = ~ Species)