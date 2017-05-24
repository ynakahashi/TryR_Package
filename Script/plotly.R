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
work_dir <- "/Users/nakahashi/Desktop/GitTest/TryR_Package/Output"
setwd(work_dir)

## install & load plotly
# install.packages("plotly")
library(plotly)

################################################################################
## Run sample script
################################################################################
## histogram
plot_ly(economics, x = ~ pop)

## scatter plot with line
plot_ly(economics, x = ~ date, y = ~ pop)

## heat map
plot_ly(z = ~ volcano)

## 3D plot
plot_ly(z = ~ volcano, type = "surface")

## line chart in 2 ways
add_lines(plot_ly(economics, x = ~ date, y = ~ unemploy/pop))
economics %>% 
   plot_ly(x = ~ date, y = ~ unemploy/pop) %>% 
   add_lines()

## multiple line
plot_ly(economics, x = ~ date, color = I("black")) %>%
   add_lines(y = ~ uempmed) %>%
   add_lines(y = ~ psavert, color = I("red"))

## scatter plot for iris and ...
p <- plot_ly(iris, x = ~ Sepal.Width, y = ~ Sepal.Length) 

## different marker size, color, symbol and line
add_markers(p, color = ~ Petal.Length, size = ~ Petal.Length)
add_markers(p, color = ~ Species)
add_markers(p, color = ~ Species, colors = "Set1")
add_markers(p, symbol = ~ Species)
add_paths(p, linetype = ~ Species)

## export
p <- economics %>% 
   plot_ly(x = ~ date, y = ~ unemploy/pop) %>% 
   add_lines()


# install.packages("htmlwidgets")
library(htmlwidgets)
saveWidget(p, "sample.html")

