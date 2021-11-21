library(dplyr) # A Grammar of Data Manipulation
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(lubridate) # Make Dealing with Dates a Little Easier
library(tidyr) # Tidy Messy Data
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(purrr) # Functional Programming Tools
library(stringr) # Simple, Consistent Wrappers for Common String Operations
library(readr) # Read Rectangular Text Data

# Create the variables for a data set.
questions <- c("Will attend college","Hangs out with nice people","Will work during high school","Is active enough","Eats a well balanced diet")
parents <- c(.89,.80,.68,.54,.31)
students <- c(.58,.85,.77,.64,.38)

# Save all hex color values as objects for easier interpretation.
orange <- "#f28242"
grey <- "#a7a7a7"
dark <- "#696969"

# Add a vector of colors to match responses.
line_color <- c(orange,grey,grey,grey,grey)
label_color <- c(orange,dark,dark,dark,dark)

# Combine all vectors into data set.
slopedata <- tibble(questions,parents,students,line_color,label_color)

ggplot(slopedata) +
  geom_segment(aes(x = 0, y = parents, xend = 1, yend = students), size = 2, color = line_color) +
  geom_segment(data = NULL, aes(x = 0, y = .89, xend = 1, yend = .58), size = 2, color = orange) +
  geom_point(aes(x = 0, y = parents), colour = line_color, size = 4) +
  geom_point(aes(x = 1, y = students), colour = line_color, size = 4) +
  scale_x_continuous(limits = c(-1,1.2), breaks = c(0,1), labels = c("Parents", "Students")) +
  annotate(geom = "text", y = parents, x = -0.05, label = paste(questions, scales::percent(parents, accuracy = 1)), hjust = 1) +
  annotate(geom = "text", y = students, x = 1.05, label = scales::percent(students, accuracy = 1), hjust = 0) +
  ylim(0,0.9) +
  theme_void() +
  theme(axis.text.x = element_text(size=12))
