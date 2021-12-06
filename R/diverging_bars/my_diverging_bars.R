library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(tidytuesdayR) # Access the Weekly 'TidyTuesday' Project Dataset

# drwho_ratings <- tt_load("2021-11-23")$imdb

drwho_seasons <- drwho_ratings %>% 
  filter(ep_num != 0) %>% 
  mutate(rating_standardised = case_when(
    rating < min(rating)+1.475 ~ "Awful",
    rating < min(rating)+(1.475*2) ~ "Bad",
    rating < min(rating)+(1.475*3) ~ "Good",
    rating <= min(rating)+(1.475*4) ~ "Excellent"
  )) %>% 
  count(season, rating_standardised, name = "value") %>% 
  with_groups(season, ~mutate(.x, perc = value/sum(value), label = scales::percent(perc, accuracy = 1))) %>% 
  mutate(rating_standardised = factor(rating_standardised, levels = c("Excellent", "Good", "Awful", "Bad")), season_name = paste("Season", season))

neg_data <- drwho_seasons %>% 
  filter(rating_standardised %in% c("Bad", "Awful")) %>% 
  mutate(perc = perc*-1) %>% 
  with_groups(season, mutate, label_position = case_when(
    rating_standardised == "Awful" ~ perc/2+lead(perc),
    rating_standardised == "Bad" ~ perc/2,
  ))

pos_data <- drwho_seasons %>% 
  filter(rating_standardised %in% c("Good", "Excellent")) %>% 
  with_groups(season, mutate, label_position = case_when(
    rating_standardised == "Excellent" ~ perc/2+lead(perc),
    rating_standardised == "Good" ~ perc/2,
  )) %>% 
  mutate(label_colour = case_when(
    rating_standardised == "Excellent" ~ "white",
    TRUE ~ "black"
  ))

terrible <- "red"
excellent <- "#003B6F"
bad <- "pink"
good <- "lightblue"

ggplot(drwho_seasons, aes(x = forcats::fct_reorder(season_name, -season), fill = rating_standardised)) +
  geom_bar(data = pos_data, aes(y = perc), 
           stat = "identity", position = "stack", 
           width = 0.45, color = "white", size = 0.5) +
  geom_bar(data = neg_data, aes(y = perc), 
           stat = "identity", position = "stack", 
           width = 0.45, color = "white", size = 0.5) +
  coord_flip() +
  geom_segment(aes(x = 12.5, xend = 0.5, y = 0, yend = 0), color = "lightgrey", size = .2) +
  scale_fill_manual(values = c(terrible,bad,excellent,good)) +
  scale_y_continuous(limits = c(-1.3, 1.1)) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    axis.text = element_blank()
  ) +
  labs(
    title = "Doctor Who was popular with viewers for ten seasons, but\nthey've really soured on the latest seasons",
    caption = "Average IMDB rating of each post-2005 Doctor Who episode (excluding the Specials), grouped into categories. Data via TidyTuesday"
  ) +
  annotate("text", x = neg_data$season_name, y = neg_data$label_position, 
           label = neg_data$label, fontface = "bold", size = 3.5) +
  annotate("text", x = pos_data$season_name, y = pos_data$label_position, 
           label = pos_data$label, fontface = "bold", size = 3.5, colour = pos_data$label_colour) +
  annotate("text", x = 12, y = -1.12, label = "Season 1", size = 4, fontface = "bold") +
  annotate("text", x = 11, y = -1.12, label = "Season 2", size = 4, fontface = "bold") +
  annotate("text", x = 10, y = -1.12, label = "Season 3", size = 4, fontface = "bold") +
  annotate("text", x = 9, y = -1.12, label = "Season 4", size = 4, fontface = "bold") +
  annotate("text", x = 8, y = -1.12, label = "Season 5", size = 4, fontface = "bold") +
  annotate("text", x = 7, y = -1.12, label = "Season 6", size = 4, fontface = "bold") +
  annotate("text", x = 6, y = -1.12, label = "Season 7", size = 4, fontface = "bold") +
  annotate("text", x = 5, y = -1.12, label = "Season 8", size = 4, fontface = "bold") +
  annotate("text", x = 4, y = -1.12, label = "Season 9", size = 4, fontface = "bold") +
  annotate("text", x = 3, y = -1.12, label = "Season 10", size = 4, fontface = "bold") +
  annotate("text", x = 2, y = -1.12, label = "Season 11", size = 4, fontface = "bold") +
  annotate("text", x = 1, y = -1.12, label = "Season 12", size = 4, fontface = "bold") +
  annotate("text", x = 12, y = .76,
           label = "Excellent",
           fontface = "bold", size = 3.5, vjust = -1.8) +
  annotate("text", x = 12, y = .27,
           label = "Good",
           fontface = "bold", size = 3.5, vjust = -1.8) +
  annotate("text", x = 11, y = -.11,
           label = "Bad",
           fontface = "bold", size = 3.5, vjust = -1.8) +
  annotate("text", x = 2, y = -.77,
           label = "Awful",
           fontface = "bold", size = 3.5, vjust = -1.8) 
