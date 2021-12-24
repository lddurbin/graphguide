library(dplyr) # A Grammar of Data Manipulation
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(lubridate) # Make Dealing with Dates a Little Easier
library(tidyr) # Tidy Messy Data
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(purrr) # Functional Programming Tools
library(stringr) # Simple, Consistent Wrappers for Common String Operations

df <- tribble(
  ~x, ~y,
  0, 0,
  1, 1.
)

img <- png::readPNG(here::here("R/colours/aklc.png"))
g <- grid::rasterGrob(img, width = .3, interpolate=TRUE)

ggplot(df) +
  annotation_custom(g, xmin=0.21, xmax=0.81, ymin=-0.1, ymax=0.05) +
  geom_text(aes(x = .175, y = 0.95, label = "Primary colour palette"), size = 6, fontface = "bold") +
  geom_rect(aes(xmin = 0, xmax = 0.17,   ymin = 0.725, ymax = 0.9),   fill = "#007CB9", colour = "black") +
  geom_text(aes(x = .075, y = 0.625, label = "Shore\nCalm, sanctuary")) +
  geom_rect(aes(xmin = 0.2075, xmax = 0.3775,   ymin = 0.725, ymax = 0.9),   fill = "#00304B", colour = "black") +
  geom_text(aes(x = .295, y = 0.625, label = "Ocean\nExploration, frontier")) +
  geom_rect(aes(xmin = 0.415, xmax = 0.585,   ymin = 0.725, ymax = 0.9),   fill = "#F8F8F8", colour = "black") +
  geom_text(aes(x = .51, y = 0.625, label = "Off white\nBeing, balance")) +
  geom_text(aes(x = .2, y = 0.43, label = "Secondary colour palette"), size = 6, fontface = "bold") +
  geom_rect(aes(xmin = 0, xmax = 0.17,   ymin = 0.2, ymax = 0.37),   fill = "#6E963C", colour = "black") +
  geom_text(aes(x = .075, y = 0.1, label = "Leaf\nNature, sustainability")) +
  geom_rect(aes(xmin = 0.2075, xmax = 0.3775,   ymin = 0.2, ymax = 0.37),  fill = "#CC423E", colour = "black") +
  geom_text(aes(x = .295, y = 0.1, label = "Nikau berry\nAllure, prominence")) +
  geom_rect(aes(xmin = 0.415, xmax = 0.585,   ymin = 0.2, ymax = 0.37),  fill = "#C0D67A", colour = "black") +
  geom_text(aes(x = .51, y = 0.1, label = "Fields\nGrowth, healing")) +
  geom_rect(aes(xmin = 0.6225, xmax = .7925,   ymin = 0.2, ymax = 0.37),  fill = "#87C9DD", colour = "black") +
  geom_text(aes(x = .71, y = 0.1, label = "Sky\nGuidance, vision")) +
  geom_rect(aes(xmin = 0.83, xmax = 1,   ymin = 0.2, ymax = 0.37),  fill = "#E76317", colour = "black") +
  geom_text(aes(x = .9, y = 0.1, label = "Sunset\nRest, horizon")) +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(-0.05,1)) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    title = element_text(family = "Arial", size = 15)
  ) +
  labs(title = "Te Kunihera o Tāmaki Makaurau brand colours\nand their associations")
