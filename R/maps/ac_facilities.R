library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(rnaturalearth) # World Map Data from Natural Earth
library(sf) # Simple Features for R

world <- ne_countries(scale = "large", returnclass = "sf")

data <- readr::read_csv(
  "R/maps/Connected Communities Service Status Update Board.csv",
  col_select = c(1:3, 15:16),
  col_types = "cccdd",
  name_repair = janitor::make_clean_names
  )

(sites <- st_as_sf(data, coords = c("long", "lat"), 
                   crs = 4326, agr = "constant"))

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = sites, aes(fill = current_service_level), size = 2, shape = 23) +
  coord_sf(xlim = c(174, 175.8), ylim = c(-36.1, -37.3), expand = FALSE)

