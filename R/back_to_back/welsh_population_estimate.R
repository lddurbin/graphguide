library(dplyr, warn.conflicts = FALSE)
library(janitor, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(ggtext, warn.conflicts = FALSE)

# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
get_the_data <- function(filename, sheet, sex) {
  readxl::read_excel(here::here(paste0("back_to_back/", filename, ".xls")), sheet = sheet, skip = 7) %>% 
    filter(Name %in% c("WALES")) %>% 
    mutate(sex = sex) %>% 
    select(-c(1:4))
}

males_2020 <- get_the_data("ukpopestimatesmid2020on2021geography", sheet = 8, sex = "male")
females_2020 <- get_the_data("ukpopestimatesmid2020on2021geography", sheet = 9, sex = "female") 

welsh_pop <- bind_rows(males_2020, females_2020) %>% 
  pivot_longer(1:ncol(.)-1, names_to = "age", values_to = "population") %>% 
  mutate(
    age = str_replace(age, "\\+", "") %>% as.double(),
    age_group = case_when(
      age >= 0 & age < 5 ~ "0-4",
      age >= 5 & age < 10 ~ "05-09",
      age >= 10 & age < 15 ~ "10-14",
      age >= 15 & age < 20 ~ "15-19",
      age >= 20 & age < 25 ~ "20-24",
      age >= 25 & age < 30 ~ "25-29",
      age >= 30 & age < 35 ~ "30-34",
      age >= 35 & age < 40 ~ "35-39",
      age >= 40 & age < 45 ~ "40-44",
      age >= 45 & age < 50 ~ "45-49",
      age >= 50 & age < 55 ~ "50-54",
      age >= 55 & age < 60 ~ "55-59",
      age >= 60 & age < 65 ~ "60-64",
      age >= 65 & age < 70 ~ "65-69",
      age >= 70 & age < 75 ~ "70-74",
      age >= 75 & age < 80 ~ "75-79",
      age >= 80 ~ "80+"
    )
  ) %>% 
  with_groups(c(age_group, sex), summarise, population = sum(population)) %>% 
  pivot_wider(names_from = sex, values_from = population)

welsh_pop_desc <- welsh_pop %>% arrange(age_group)

welsh_pop_data <- welsh_pop_desc %>% 
  pivot_longer(2:3, names_to = "variable") %>% 
  mutate(
    value = case_when(
      variable == "male" ~ value*-1,
      TRUE ~ value
    ),
    abs.value = abs(value),
    bar.colour = case_when(
      variable == "male" ~ "#ffca72",
      variable == "female" ~ "#ff9642"
    ),
    label = as.character(prettyNum(abs.value, big.mark = ",")),
    abs.position = case_when(
      abs.value > 20000 ~ 4000,
      TRUE ~ abs.value + 2000
    ),
    position = case_when(
      variable == "male" ~ abs.position*-1,
      TRUE ~ abs.position
    ),
    hjust = case_when(
      variable == "male" ~ 1,
      TRUE ~ 0
    ),
    label.colour = case_when(
      abs.value < 20000 ~ "black",
      variable == "male" ~ "black",
      variable == "female" ~ "white"
    )
  )

welsh_pop_data_male <- welsh_pop_data %>% filter(variable == "male")
welsh_pop_data_female <- welsh_pop_data %>% filter(variable == "female")

plot <- ggplot() +
  geom_bar(
    data = welsh_pop_data_male, 
    stat = "identity", 
    position = "identity", 
    aes(x = age_group, 
        y = value, 
        fill = variable)
  ) +
  geom_bar(
    data = welsh_pop_data_female, 
    stat = "identity", 
    position = "identity", 
    aes(x = age_group, 
        y = value, 
        fill = variable)
  ) +
  coord_flip() +
  scale_x_discrete(limits = welsh_pop$age_group) +
  theme_void() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(family = "sans", size = 12),
    axis.title.y = element_text(family = "sans", size = 15, colour = "white"),
    title = element_text(size = 14)
  ) +
  annotate(
    "text",
    label = welsh_pop_data$label,
    x = welsh_pop_data$age_group,
    y = welsh_pop_data$position,
    color = welsh_pop_data$label.colour,
    family = "sans",
    size = 4,
    hjust = welsh_pop_data$hjust
  ) +
  labs(
    x = "HI",
    title = "Among the 80+ age group living in Wales in 2020, <span style='color:blue'><strong>women</strong></span> outnumbered<br><span style='color:orange'><strong>men</strong></span> by more than 30,000",
    caption = "Data: ONS (https://bit.ly/3lWgRou)"
    ) +
  scale_fill_manual(values = c(male = "orange", female = "blue")) +
  theme(plot.title = element_markdown(lineheight = 1.1))

png(file = here::here("back_to_back/welshpop.png"), bg = "white", height = 785, width = 816, res = 100)
plot(plot)
dev.off()
