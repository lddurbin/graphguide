library(dplyr, warn.conflicts = FALSE)
library(janitor, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)

uk_cities <- read_csv(here::here("Power BI/maps/uk_cities_population.csv"), col_select = c(1,5,7), col_types = "ccc") %>% 
  clean_names() %>% 
  mutate(
    population = word(population),
    city = str_remove_all(city_1, "[:digit:]") %>% str_extract("[^\\(]+") %>% str_remove_all("[:punct:]"),
    nation_region = word(nation_region, -1),
    nation_region = case_when(nation_region == "Ireland" ~ "Northern Ireland", TRUE ~ nation_region)
  ) %>% 
  mutate(population_extract = word(population, -1, sep = fixed(",")), population_extract_modified = case_when(
    str_length(population_extract) > 3 ~ str_sub(population_extract, 1, 3),
    TRUE ~ population_extract
  )) %>% 
  mutate(population_city = case_when(
    population_extract != population_extract_modified ~ paste(word(population, 1, -2, sep = fixed(",")), population_extract_modified, sep = ","),
    TRUE ~ population
  )) %>% 
  mutate(population_city = str_remove_all(population_city, "[:punct:]") %>% as.double()) %>% 
  select(city, country = nation_region, population_city)

uk_countries <- read_csv(here::here("Power BI/maps/uk_countries_population.csv"), col_select = c("Name", "Population (2019)")) %>% 
  clean_names() %>% 
  mutate(population_country = word(population_2019) %>% str_remove_all("[:punct:]") %>% as.double(), name = word(name), .keep = "unused") %>% 
  head(4) %>% 
  mutate(country = case_when(
    name == "Northern" ~ "Northern Ireland",
    TRUE ~ name
  ), .keep = "unused")

uk_population_data <- left_join(uk_countries, uk_cities, by = "country")

write_excel_csv(uk_population_data, here::here("Power BI/maps/uk_population_data.csv"))