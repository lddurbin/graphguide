library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggforce)

file <- fs::dir_ls(here::here("R/comets"), glob = "*.csv")

physical_checkouts <- readr::read_csv(file, col_types = "ccccd", name_repair = janitor::make_clean_names) %>% 
  mutate(date = if_else(financial_year == "FY18", "jan_2018", "jan_2022"), location_name = case_when(
      location_name == "Massey" & date == "Jan 2018" ~ "Te Manawa",
      location_name == "Leys Institute" & date == "Jan 2018" ~ "Leys Institute Little",
      TRUE ~ location_name)) %>% 
  filter(financial_year %in% c("FY18", "FY22") & month == "Jan" & !local_board %in% c("Mobile Libraries", "Other") & sum_of_checkouts_actual > 0) %>% 
  select(-c(financial_year, month)) %>% 
  tidyr::pivot_wider(names_from = date, values_from = sum_of_checkouts_actual) %>% 
  mutate(colour = if_else(jan_2018 > jan_2022, "orange", "blue"))

