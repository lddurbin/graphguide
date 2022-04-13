library(dplyr) # A Grammar of Data Manipulation
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(lubridate) # Make Dealing with Dates a Little Easier
library(tidyr) # Tidy Messy Data
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(purrr) # Functional Programming Tools
library(stringr) # Simple, Consistent Wrappers for Common String Operations

data <- readr::read_csv("R/waterfall/kitchen_reno.csv")

plot_data <- data %>% mutate(
  last_value = replace_na(lag(value), 0),
  last_value = if_else(category == "Actual", 0, last_value),
  label = value - last_value,
  label_coordinates = if_else(label >= 0, value + 1.5, value - 1),
  label_vjust = if_else(label >= 0, 0, 1),
  index = nrow(data):1,
  colour = if_else(label >= 0, "firebrick3", "dodgerblue2"),
  colour = if_else(category %in% c("Original", "Actual"), "grey60", colour)
)

ggplot(
  plot_data, 
  aes(x = index, y = value), 
  stat = "identity", position = "identity"
) +
  geom_rect(aes(
    xmin = index - 0.3, xmax = index + 0.3, 
    ymin = last_value, ymax = value, 
    fill = colour
  )) +
  scale_x_discrete(
    label = rev(plot_data$category), limits = plot_data$category, breaks = plot_data$category
  ) +
  scale_fill_manual(
    values = c("firebrick3", "dodgerblue2", "grey60"), 
    limits = c("firebrick3", "dodgerblue2", "grey60")
  ) +
  annotate(
    "text", 
    x = plot_data$index, y = plot_data$label_coordinates, 
    label = plot_data$label, vjust = plot_data$label_vjust,
    size = 5
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 12),
    axis.text.x = element_blank(),
    plot.title.position = "plot",
    title = element_text(size = 14),
  ) +
  labs(
    title = "It took more than twice as many days as planned to complete our kitchen renovation"
  )
