library(dplyr)
library(ggplot2)
library(ggtext)
library(stringr)

hospital_stay <- tibble::tribble(
  ~"Quarter",  ~"<=24",	  ~"24 and 36",	   ~"36 and 48", ~"48 and 59", ~">=60",	~"Unknown",
  "2019/Q1",	 0.122,	    0.539,	         0.063,	       0.189,	       0.069,	  0.018,
  "2019/Q2",	 0.143,	    0.583,	         0.041,	       0.167,	       0.059,	  0.007,
  "2019/Q3",	 0.195,	    0.522,	         0.028,	       0.174,	       0.07,    0.011,
  "2019/Q4",	 0.254,	    0.503,           0.027,	       0.144,	       0.056,   0.017
  )

fill_values <- c(
  "<=24" = "#4F81BD",
  "24 and 36" = "#C04F4D",
  "36 and 48" = "#9BBB59",
  "48 and 59" = "#8064A2",
  ">=60" = "#4BACC6",
  "Unknown" = "#F79646"
  )

hosital_stay_reshaped <- hospital_stay |> 
  tidyr::pivot_longer(col = -Quarter) |> 
  mutate(
    name = factor(
      name,
      levels = c("<=24", "24 and 36", "36 and 48", "48 and 59", ">=60", "Unknown")
    ),
    position = str_sub(Quarter, -1) |> as.double(),
    emphasis = if_else(name == "<=24", TRUE, FALSE),
    tooltip = paste0(Quarter, ": ", scales::label_percent(accuracy=1)(value))
  ) 

hospital_stay_aggregated <- hospital_stay |> 
  mutate(
    more_than_a_day = rowSums(across(3:4)),
    more_than_two_days = rowSums(across(5:6)),
    quarter = word(Quarter, 2, sep = fixed("/"))
    ) |> 
  select(quarter, a_day_or_less = "<=24", more_than_a_day, more_than_two_days) |> 
  tidyr::pivot_longer(cols = 2:4) |> 
  filter(quarter %in% c("Q1", "Q4")) |> 
  tidyr::pivot_wider(names_from = quarter, values_from = value) |> 
  mutate(name = case_when(
    name == "a_day_or_less" ~ "A day\nor less:",
    name == "more_than_a_day" ~ "More than a\nday, less\nthan two:",
    name == "more_than_two_days" ~ "Two days\nor more:"
  ))


ggplot(data = hospital_stay_aggregated) +
  geom_segment(aes(x = 0, y = Q1, xend = 1, yend = Q4, colour = name), size = 3) +
  geom_point(aes(x = 0, y = Q1), colour = "black", size = 6) +
  geom_point(aes(x = 0, y = Q1, colour = name), size = 5) +
  geom_point(aes(x = 1, y = Q4), colour = "black", size = 6) +
  geom_point(aes(x = 1, y = Q4, colour = name), size = 5) +
  geom_text(aes(
      x = -0.08,
      y = Q1,
      label = paste0(name, "\n", scales::percent(Q1, accuracy = 1.1))),
      size = 4.5
    ) +
  geom_text(aes(
    x = 1.05,
    y = Q4,
    label = scales::percent(Q4, accuracy = 1.1)),
    size = 4.5
  ) +
  scale_y_continuous(limits = c(0,.7), labels = scales::percent, breaks = c(0,.2,.4,.6)) +
  scale_x_continuous(limits = c(-0.08,1.05), breaks = c(0,1), labels = c("Q1", "Q4")) +
  scale_colour_manual(values = c("#f1a92b", "#99f4f8", "#c4efeb")) +
  labs(
    title = "By the end of 2019, more than a quarter of patients were being discharged from<br>hospital <span style='color:#f1a92b'>within a day of completing surgery</span>"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_markdown(size=20),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size=16),
    axis.text.y = element_blank(),
    axis.title = element_blank()
  )
  