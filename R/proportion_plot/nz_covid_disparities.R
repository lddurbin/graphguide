suppressPackageStartupMessages(library(dplyr)) # A Grammar of Data Manipulation
suppressPackageStartupMessages(library(janitor)) # Simple Tools for Examining and Cleaning Dirty Data
suppressPackageStartupMessages(library(ggplot2)) # Create Elegant Data Visualisations Using the Grammar of Graphics
suppressPackageStartupMessages(library(tidyr)) # Tidy Messy Data
suppressPackageStartupMessages(library(purrr)) # Functional Programming Tools
suppressPackageStartupMessages(library(scales)) # Scale Functions for Visualization

source(here::here("R/proportion_plot/create_vector.R"))

calculate_y_value <- function(metric, ethnicity) {
  population_ethnicity <- covid_ethnicity %>% pull(population_cumulative, name = prioritised_ethnicity)
  cases_metric <- covid_ethnicity %>% pull({{metric}}, name = prioritised_ethnicity)
  
  create_slope_vector(num_values = 16, start_value = cases_metric[ethnicity], end_value = population_ethnicity[ethnicity])
}

label_calc_left <- function(y_value, lag_value) {
  diff <- ifelse(y_value == "european_or_other_y_value", 0.2, min(covid_cases[[y_value]])-min(covid_cases[[lag_value]]))
  min(covid_cases[[y_value]])-(diff)/2
}

label_calc_right <- function(y_value, lag_value) {
  diff <- ifelse(y_value == "european_or_other_y_value", 0.63, max(covid_cases[[y_value]])-max(covid_cases[[lag_value]]))
  max(covid_cases[[y_value]])-(diff)/2
}

write_label <- function(ethnicity, percent, side) {
  ethnicity <- ifelse(ethnicity == 1, "European\nor Other", covid_ethnicity$prioritised_ethnicity[ethnicity])
  value = ifelse(side == "left", covid_ethnicity$percentage_of_all_cases[percent], population_ethnicity$population[percent])
  paste0(ethnicity, ": ", percent(value))
}

# https://github.com/minhealthnz/nz-covid-data
hsu_population <- readRDS(here::here("R/proportion_plot/hsu_population_file.rds"))

population_ethnicity <- hsu_population %>%
  clean_names() %>% 
  mutate(ethnic_group = ifelse(ethnic_group %in% c("Unknown", "Various"), "European or Other", ethnic_group)) %>% 
  with_groups(ethnic_group, summarise, population = sum(population)) %>% 
  adorn_percentages("col")

# https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-case-demographics
covid_ethnicity <- readr::read_csv(here::here("R/proportion_plot/nz_covid_ethnicity.csv"), col_types = "cdddddd") %>% 
  arrange(order) %>% 
  adorn_rounding(digits = 2) %>% 
  mutate(population_cumulative = cumsum(population), cases_cumulative = cumsum(percentage_of_all_cases), hospitalisations_cumulative = cumsum(percentage_of_all_hospitalised_cases))

covid_cases <- tibble(
  x_percent = seq(0,1, by = 1/15),
  european_or_other_y_value = calculate_y_value(cases_cumulative, "European or Other"),
  asian_y_value = calculate_y_value(cases_cumulative, "Asian"),
  pacific_y_value = calculate_y_value(cases_cumulative, "Pasifika"),
  maori_y_value = calculate_y_value(cases_cumulative, "Māori")
  )

ethnicities <- c("maori_y_value", "pacific_y_value", "asian_y_value", "european_or_other_y_value")

ggplot(covid_cases, aes(x = x_percent)) +
  geom_text(aes(x = -.08, y = 1.05, label = "Share of\nCOVID-19 cases")) +
  geom_text(aes(x = 1.08, y = 1.05, label = "Share of\nNZ population")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1.1), expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), title = element_text(size=15)) +
  labs(
    title = "The COVID-19 Delta outbreak in New Zealand has\ndisproportionately affected Māori and Pacifika populations",
    caption = "COVID-19 community cases since 16 August 2021 & NZ population data"
  ) +
  map2(ethnicities, c("#532476", "#7030a0", "#8a3cc4", "#9e5ece"), ~geom_area(aes(y = .data[[{{.x}}]]), fill = .y) ) + # plot areas
  map(ethnicities, ~geom_line(aes(y = .data[[{.x}]]), size = 5/4)) + # add line above each area
  map(ethnicities, ~geom_segment(aes(x = -.18, xend = 0, y = min(.data[[{.x}]]), yend = min(.data[[{.x}]])), size = 5/4)) + # extend the lines to the left
  map(ethnicities, ~geom_segment(aes(x = 1, xend = 1.18, y = max(.data[[{.x}]]), yend = max(.data[[{.x}]])), size = 5/4)) + # extend the lines to the right
  pmap(list(ethnicities, c(ethnicities[2:4], ""), c(4,3,2,1)), ~geom_text(aes(x = -.09, y = label_calc_left(..1, ..2), label = write_label(..3, ..3, "left")))) + # add labels on the left
  pmap(list(ethnicities, c(ethnicities[2:4], ""), c(4,3,2,1), c(3,4,1,2)), ~geom_text(aes(x = 1.09, y = label_calc_right(..1, ..2), label = write_label(..3, ..4, "right")))) # add labels on the right
