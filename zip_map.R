#Zip Map
#http://homepage.divms.uiowa.edu/~luke/classes/STAT4580-2020/maps.html#zip-code-boundaries
library(sf)
library(tigris)
library(ggplot2)
setwd("/Users/SBruce 1/repos/CovidAnalysis/") #Set working Directory




z_sf <- zctas(cb = TRUE, starts_with = c("50", "51", "52"), class = "sf")