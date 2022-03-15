library(dplyr) # A Grammar of Data Manipulation
library(lubridate) # Make Dealing with Dates a Little Easier
library(tidyr) # Tidy Messy Data
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(purrr) # Functional Programming Tools

add_constants <- function(value) {
  seq(ymd("2018-07-01"), ymd("2019-06-01"), by = "1 month") %>%
    month(label = TRUE, abbr = FALSE) %>%
    tibble::as_tibble_col(column_name = "month") %>%
    mutate(value := {{value}}) %>% 
    pivot_wider(names_from = month, values_from = value)
}

here <- here::here("R/radial")

glen_eden <- readr::read_csv(fs::dir_ls(here, glob = "*.csv"), name_repair = janitor::make_clean_names) %>% 
  mutate(year = if_else(quarter %in% c("Q1", "Q2"), 2018, 2019), date = ymd(paste(year, month, "01", sep = "-")), month = month(date, label = TRUE, abbr = FALSE)) %>%
  select(month, in_library_visits) %>% 
  pivot_wider(names_from = month, values_from = in_library_visits)

radial_data <- map_dfr(c(max(glen_eden), 0), add_constants) %>% 
  bind_rows(glen_eden)

fmsb::radarchart(
  radial_data[rev(colnames(radial_data))],
  pcol = "#007CB9",                      ## Plot border color.                   
  pfcol = scales::alpha("#007CB9", 0.4), ## Plot fill color, with 40% opacity.
  plwd = 6,                                   ## Plot border color width.
  cglcol = "darkgray", ## Line color
  cglty = 1,           ## Designation for solid line (please experiment!)
  cglwd = 2            ## Line width
  )
