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
    label = case_when(
      abs.value > 0.01 ~ paste(abs.value*100, "%", sep = ""),
      TRUE ~ "not in top 10"
    ),
    abs.position = case_when(
      abs.value > 0.04 ~ 0.005,
      TRUE ~ abs.value + 0.005
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
      abs.value > 0.01 ~ "black",
      TRUE ~ bar.colour
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
  scale_x_discrete(limits = pop.data.desc$condition) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "sans", size = 10),
    panel.border = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_manual(values = c(male = "#ffca72", female = "#ff9642")) +
  annotate(
    "text",
    label = pop.data$label,
    x = pop.data$condition,
    y = pop.data$position,
    color = pop.data$label.colour,
    family = "sans",
    size = 3,
    hjust = pop.data$hjust
  ) +
  annotate("text", x = 12, y = -0.005, label = "Male", 
           color =  "#ffca72", size = 3.5, hjust = 1, vjust = 1,
           fontface = "bold", family = "sans") +
  annotate("text", x = 12, y = 0.005, label = "Female", 
           color = "#ff9642", size = 3.5, hjust = 0, vjust = 1,
           fontface = "bold", family = "sans") +
  ggtitle("Leading causes of death for males and females are similar, with a few \nnotable exceptions.") +
  theme(
    plot.title=element_text(hjust = 0, vjust = 1, size = 12, face = "bold")
  )
