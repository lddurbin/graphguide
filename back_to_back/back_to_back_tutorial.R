library(dplyr, warn.conflicts = FALSE)
library(janitor, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)

pop.data.desc <- readr::read_csv(here::here("back_to_back/population-pyramid.csv"), col_types = "cdd") %>% 
  arrange(male)

pop.data <- pop.data.desc %>% 
  pivot_longer(2:3, names_to = "variable") %>% 
  mutate(value = case_when(
    variable == "male" ~ value*-1,
    TRUE ~ value
  ),
  bar.colour = case_when(
    variable == "male" ~ "#ffca72",
    variable == "female" ~ "#ff9642"
  )
  )

pop.data.male <- pop.data %>% filter(variable == "male")
pop.data.female <- pop.data %>% filter(variable == "female")

ggplot() +
  geom_bar(
    data = pop.data.male, 
    stat = "identity", 
    position = "identity", 
    aes(x = condition, 
        y = value, 
        fill = variable)
  ) +
  geom_bar(
    data = pop.data.female, 
    stat = "identity", 
    position = "identity", 
    aes(x = condition, 
        y = value, 
        fill = variable)
  ) +
  scale_y_continuous(breaks = seq(-1,1,.1)) +
  coord_flip() +
  scale_x_discrete(limits = pop.data.desc$condition)
