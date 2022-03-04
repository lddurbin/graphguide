library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggforce)

directory <- here::here("R/comets")

physical_checkouts <- readr::read_csv(fs::dir_ls(directory, glob = "*.csv"), col_types = "ccccd", name_repair = janitor::make_clean_names) %>% 
  mutate(date = if_else(financial_year == "FY18", "jan_2018", "jan_2022"), location_name = case_when(
      location_name == "Massey" & date == "jan_2018" ~ "Te Manawa",
      location_name == "Leys Institute" & date == "jan_2018" ~ "Leys Institute Little",
      TRUE ~ location_name)) %>% 
  filter(financial_year %in% c("FY18", "FY22") & month == "Jan" & !local_board %in% c("Mobile Libraries", "Other") & sum_of_checkouts_actual > 0 & location_name != "Takaanini") %>% 
  select(-c(financial_year, month)) %>% 
  tidyr::pivot_wider(names_from = date, values_from = sum_of_checkouts_actual) %>% 
  mutate(colour = if_else(jan_2018 > jan_2022, "orange", "blue"))

plot <- ggplot(physical_checkouts) +
  geom_link(aes(x = jan_2018, xend = jan_2022, y = location_name, yend = location_name, size = stat(index), color = colour), lineend = "round")

ggsave(filename = paste0(directory, "/Comet_PNG_version.png"), plot = plot, 
       height = 10, width = 5, units = "in", dpi = 300)
