library(dplyr) # A Grammar of Data Manipulation
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(lubridate) # Make Dealing with Dates a Little Easier
library(tidyr) # Tidy Messy Data
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(purrr) # Functional Programming Tools
library(stringr) # Simple, Consistent Wrappers for Common String Operations

cartogram <- readr::read_csv("R/maps/territorial-authority-local-board-talb-hexagon-cartogram.csv") %>% 
  select(location = 5, shape_length = 6)

overseas <- readxl::read_excel("R/maps/Table 9 Overseas Born.xlsx", skip = 6) %>% 
  select(location = 1, overseas_2013 = 2) %>% 
  mutate(location = case_when(
    location == "Christchurch City(2)" ~ "Christchurch City",
    location == "Wanganui District" ~ "Whanganui District",
    location == "Auckland(8)" ~ "Auckland",
    TRUE ~ location
  ))

joined_data <- left_join(cartogram, overseas, by = "location")
