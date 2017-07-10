################################################################################
##
## geofacet.
## written by Y.Nakahashi 
## 2017/7/10
##
################################################################################

################################################################################
### environmental settings
################################################################################
devtools::install_github("hafen/geofacet")
library(geofacet)
library(ggplot2)


################################################################################
### Run sample script
################################################################################
ggplot(state_ranks, aes(variable, rank, fill = variable)) + 
   geom_col() + 
   coord_flip() +
   facet_geo(~ state) + 
   theme_bw()

ggplot(state_unemp, aes(year, rate)) + 
   geom_line() + 
   facet_geo(~ state, grid = "us_state_grid2") + 
   scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) + 
   ylab("Unemployment Rate (%)")

ggplot(eu_gdp, aes(year, gdp_pc)) +
   geom_line(color = "steelblue") + 
   facet_geo(~ name, grid = "eu_grid1", scales = "free_y") + 
   scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) + 
   ylab("GDP Per Capita in Relation to EU Index (100)") + 
   theme_bw()
