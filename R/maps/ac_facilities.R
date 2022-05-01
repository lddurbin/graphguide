library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(sp) # Classes and Methods for Spatial Data
library(rgdal) # Bindings for the 'Geospatial' Data Abstraction Library

# facilities <- readr::read_csv("R/maps/Connected Communities Service Status Update Board.csv", col_select = c(1:3, 15:16), col_types = "cccdd", name_repair = janitor::make_clean_names)
# 
# coordinates(facilities) <- c("lat", "long")
# 
# writeOGR(facilities, "R/maps", "ac_facilities", driver = "ESRI Shapefile")

facilities_sp <- readOGR("R/maps/", "ac_facilities")

plot(facilities_sp)

