# Load the necessary packages.
library(ggplot2, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

# Use the read.csv() function to import the file from the directory where it is located on your computer.
data <- read_csv(here::here("legends/LineLabelsWorksheet.csv"))

# Use the melt() function to reformat the data, and assign the data to a new object.
format_data <- data %>% pivot_longer(cols = 2:11, names_to = "variable")

# Create a new data frame that includes all 2015 observations.
label_data <- format_data %>% filter(variable == "2015")

# Rearrange the new data set alphabetically by the 'type' column.
label_data <- label_data %>% arrange(type)

# Create a column named "palette" for each color.
# Order the colors as you'd like to apply them from observations 1-4.
label_data <- label_data %>% mutate(palette  = c("red","forestgreen","orange","blue"))

# Create another new column named "labels" for each label.
label_data <- label_data %>% mutate(labels  = c("A","B","C","D"))

ggplot(data = format_data, aes(x = variable, y = value, group = type, color = type)) +
  geom_line(size = 2) +
  annotate("text", x = label_data$variable, y = label_data$value, label = label_data$labels, color = label_data$palette, hjust = -0.5) +
  scale_color_manual(values = label_data$palette) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", size = 0.5),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  scale_y_continuous(limits=c(0,9), breaks=seq(0,9,1))