library(dplyr) # A Grammar of Data Manipulation
library(tidyr) # Tidy Messy Data
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(scales) # Scale Functions for Visualization

file <- fs::dir_ls(here::here("R/diverging_bars"), glob = "*.csv")

raw_data <- readr::read_csv(file)

survey <- raw_data %>% 
  pivot_longer(2:5, names_to = "variable", values_to = "value") %>% 
  mutate(percent = percent(value, accuracy = 1), variable = factor(variable, levels = c("Strongly Agree", "Agree", "Disagree", "Strongly Disagree")))

neg_data <- survey %>% 
  filter(variable %in% c("Agree", "Strongly Agree")) %>% 
  mutate(value = value*-1) %>% 
  with_groups(description, mutate, label = case_when(
    variable == "Strongly Agree" ~ value/2+lead(value),
    variable == "Agree" ~ value/2,
  ))

pos_data <- survey %>% 
  filter(variable %in% c("Disagree", "Strongly Disagree")) %>% 
  with_groups(description, mutate, label = case_when(
    variable == "Disagree" ~ value/2,
    variable == "Strongly Disagree" ~ value+lag(value/2),
    ))

agree <- "#bacece"
strongly_agree <- "#61898a"
disagree <- "#edc1b6"
strongly_disagree <- "#d16349"

low.response <- "I should get paid well for my dataviz ninja skills."
mid.response <- "I can manipulate Excel to do what I want it to do."
top.response <- "I would call myself a data nerd."
responses <- c(low.response,mid.response,top.response)

ggplot(data = survey, aes(x = description, fill = variable)) +
  geom_bar(data = neg_data, aes(y = value), 
           stat = "identity", position = "stack", 
           width = 0.45, color = "white", size = 0.5) + 
  geom_bar(data = pos_data, aes(y = value), 
           stat = "identity", position="stack", 
           width = 0.45, color = "white", size = 0.5) +
  coord_flip() + 
  theme_bw() +
  ylim(-1.2,.3) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  scale_x_discrete(limits = responses) +
  scale_fill_manual(values = c(agree,disagree,strongly_agree,strongly_disagree)) +
  ggtitle("While feeling confident in data wrangling, more participants\nshied away from calling themselves nerds.") +
  geom_segment(aes(x = top.response, xend = low.response, y = 0, yend = 0), color = "lightgrey", size = .2) +
  annotate("text", x = neg_data$description, y = neg_data$label, 
           label = neg_data$percent, fontface = "bold", size = 3.5) +
  annotate("text", x = pos_data$description, y = pos_data$label, 
           label = pos_data$percent, fontface = "bold", size = 3.5) +
  annotate("text", x = top.response, y = -.78, 
           label = "I would call myself a data\nnerd.", 
           fontface = "bold", size = 3, hjust = 1) +
  annotate("text", x = mid.response, y = -.83, 
           label = "I can manipulate Excel to\ndo what I want it to do.", 
           fontface = "bold", size = 3, hjust = 1) +
  annotate("text", x = low.response, y = -.88, 
           label = "I should get paid well for\nmy dataviz ninja skills.", 
           fontface = "bold", size = 3, hjust = 1) +
  annotate("text", x = top.response, y = -.525, 
           label = "Strongly\nAgree", 
           fontface = "bold", size = 2.5, vjust = -3) +
  annotate("text", x = top.response, y = -.15, 
           label = "Agree", 
           fontface = "bold", size = 2.5, vjust = -10) +
  annotate("text", x = top.response, y = .075, 
           label = "Disagree", 
           fontface = "bold", size = 2.5, vjust = -10) +
  annotate("text", x = top.response, y = .2, 
           label = "Strongly\nDisagree", 
           fontface = "bold", size = 2.5, vjust = -3)

