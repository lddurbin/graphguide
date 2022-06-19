library(dplyr) # A Grammar of Data Manipulation
library(readxl) # Read Excel Files
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(tidyr) # Tidy Messy Data
library(ggtext) # Improved Text Rendering Support for 'ggplot2'

# Source: https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-vaccine-data
vaccinations <- read_excel(here::here("R/lines/covid_vaccinations_12_10_2021-v2.xlsx"), sheet = 5) %>% 
  clean_names() %>% 
  pivot_longer(2:3, names_to = "dose") %>% 
  mutate(date = as.Date(date))

dose_labels <- vaccinations %>% 
  filter(date == max(date)) %>% 
  arrange(dose) %>% 
  mutate(palette = c("orange", "blue"), labels = c("First Dose", "Second Dose"))

range <- c(as.Date("2021-02-18"), as.Date("2021-11-30"))

vaccinations %>% 
  ggplot(aes(x = date, y = value, fill = dose, color = dose)) +
  geom_line(size = 1.5) +
  annotate("text", x = dose_labels$date, y = dose_labels$value, label = dose_labels$labels, color = dose_labels$palette, hjust = -0.2) +
  scale_color_manual(values = dose_labels$palette) +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.position = "none", panel.grid.major.x = element_blank()) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_date(limits = range, breaks = "1 month", labels = scales::label_date("%B")) +
  labs(
    title = "After peaking in September, the number of <span style='color: orange'>first doses</span><br>of the COVID-19 vaccine administered in New Zealand during<br>2021 dropped sharply as the number of <span style='color:blue'>second doses</span><br>gathered pace.",
    subtitle = "Number of COVID-19 vaccines administered in New Zealand per day",
    caption = "Data: NZ Ministry of Health (https://bit.ly/3lNzN8T)"
  ) +
  theme(plot.title = element_markdown(lineheight = 1.1))

ggsave(file = here::here("lines/NZ_vaccines.png"), dpi = 150)
