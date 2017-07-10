devtools::install_github("hafen/geofacet")
library(geofacet)
library(ggplot2)

ggplot(state_ranks, aes(variable, rank, fill = variable)) + 
   geom_col() + 
   coord_flip() +
   facet_geo(~ state) + 
   theme_bw()
