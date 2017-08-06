################################################################################
##
## Try ggvis
## written by Y.Nakahashi 
## 2017/8/3
##
################################################################################

################################################################################
### environmental settings
################################################################################
## working directory
work_dir <- "/Users/nakahashi/Desktop/GitTest/TryR_Package/Script"
setwd(work_dir)

## Library installation
# install.packages("ggvis")

## Load library
library(ggvis)

################################################################################
### Run sample script
################################################################################
demo(package = "ggvis")
demo(bar)
demo(boxplot)
demo(brush)
demo(dynamic)
demo(guides)
demo(histogram)
demo(hover)
demo(interactive)
demo(lines)
demo(scales)
demo(scatterplot)
demo(size)
demo(smooth)
demo(subvis)
demo(tile)
demo(tourr)

mtcars %>% 
   ggvis(~mpg, ~wt, fill = ~factor(cyl)) %>%
   layer_points()

mtcars %>% 
   ggvis(~factor(cyl), ~wt, fill = ~factor(cyl)) %>%
   layer_boxplots()

data.frame(x = rnorm(1000)) %>%
   ggvis(~x) %>%
   layer_histograms() %>%
   add_axis("x", title = "value", ticks = 5)


mtcars %>% 
   ggvis(~mpg, ~wt) %>%
   layer_points() %>%
   add_tooltip(function(x) paste(x, collapse = " / "))



data.frame(x = rnorm(1000)) %>%
   ggvis(~x, fill := input_select(c("red", "blue", "green"))) %>%
   layer_histograms(width = input_slider(0.01, 3, 0.1, 0.01, label="ビン幅"))


mtcars %>% 
   ggvis(x = ~wt, y = ~mpg) %>% 
   layer_points() %>% 
   layer_smooths(span = input_slider(0, 1, 0.5, 0.1, label = "span")) 


