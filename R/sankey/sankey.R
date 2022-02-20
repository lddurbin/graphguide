# devtools::install_github("ramnathv/rCharts")
library(rCharts)
library(dplyr) # A Grammar of Data Manipulation
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(lubridate) # Make Dealing with Dates a Little Easier
library(tidyr) # Tidy Messy Data
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(purrr) # Functional Programming Tools
library(stringr) # Simple, Consistent Wrappers for Common String Operations

sankey_diagram <- rCharts$new()

drwho <- readRDS("R/sankey/doctorwho.rds") %>% 
  filter(type == "episode" & season_number != 13) %>% 
  with_groups(season_number, summarise, viewers = sum(uk_viewers)) %>% 
  mutate(doctor = case_when(
    season_number == 1 ~ "Christopher Eccleston",
    season_number %in% c(2,3,4) ~ "David Tennant",
    season_number %in% c(5,6,7) ~ "Matt Smith",
    season_number %in% c(8,9,10) ~ "Peter Capaldi",
    season_number %in% c(11,12) ~ "Jodie Whittaker",
    TRUE ~ ""
  )) %>% 
  with_groups(doctor, mutate, number = sequence(n()) %>% as.character()) %>% 
  select(source = doctor, target = number, value = viewers)

sankey_diagram$set(
  data = drwho,
  nodeWidth = 15,
  nodePadding = 10,
  layout = 32,
  width = 750,
  height = 500,
  labelFormat = ".1%"
)

sankey_diagram
