# Load the necessary packages from the package library.
library(ggplot2, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

# Import the data from the directory where you saved the file on your computer.
worksheet.data <- read_csv(file = here::here("colours/CustomizeColorsWorksheet.csv"), col_types = "cdddd")

# Restructure the data.
bardata <- worksheet.data %>% 
  pivot_longer(2:6, names_to = "variable") %>% 
  mutate(
    description = factor(description, levels = c("I like exploring my data.", "I like working with Excel.", "I like talking about my data with others.", "I consider myself an Excel ninja.")),
    variable = factor(variable, levels = c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree"))
    )

# Preview the first 10 observations with the head() function.
head(bardata, n = 10)

# Begin the bar charts.
barplot <- ggplot(data = bardata, aes(x = description, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.6, size = 0.25) + 
  coord_flip() + 
  theme_bw()

# Tidy up the graph.
barplot <- barplot +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.key = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, hjust = 1),
    axis.text.x = element_text(color = "black", size = 9),
    title = element_text(size = 15)
  ) +
  labs(x = "", y = "")

# Adjust the properties of the axes.
barplot <- barplot +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(breaks=c(0,.2,.4,.6,.8,1), labels = scales::percent_format(accuracy = 1)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_line(color = "black", size = 0.3)
  )

# Alter the axis lines using geom_hline() and geom_segment() functions.
barplot <- barplot +
  theme(axis.line = element_blank()) +
  geom_hline(yintercept = 0, size = 0.15) +
  geom_segment(data = NULL, aes(x = 0.4, y = 0, xend = 0.4, yend = 1), size = 0.15, color = "black")

# Apply color.
barplot <- barplot +
  scale_fill_manual(values = c("#004879","#00A2E0","#DBE48D","#F4900F","#951610")) +
  labs(title = "Most folks don't consider themselves to be Excel ninjas, but they still like\nexploring data!")

# Display the final visualization.
barplot

# Export the file using the attributes of your choice.
ggsave(file = here::here("colours/stacked_bar_charts_corrected.png"), dpi = 150)
