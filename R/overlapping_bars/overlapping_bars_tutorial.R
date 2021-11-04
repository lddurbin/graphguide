library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)

data <- read_csv(here::here("R/overlapping_bars/overlapping-bars.csv"), col_types = "cdd") %>% 
  mutate(bucket = factor(bucket, levels = c("Bucket A", "Bucket B", "Bucket C", "Bucket D", "Bucket E")))

data %>% 
  ggplot() +
  geom_col(aes(x = forcats::fct_rev(bucket), y = budgeted, fill = "Budgeted"), width = 1/2) +
  geom_col(aes(x = forcats::fct_rev(bucket), y = actual, fill = "Actual"), width = 1/4) +
  scale_y_continuous(breaks = c(seq(from = 0, to = 160, by = 20)), labels = scales::dollar_format()) +
  scale_fill_manual(values = c("Budgeted" = "dark grey", "Actual" = "blue")) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    title = element_text(size = 15, face = "bold", color = "#373737"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(size=1),
    axis.text = element_text(size = 12, face = "bold"),
    legend.position = c(1 ,1),
    legend.justification = 1,
    legend.title = element_blank(),
    legend.direction = "horizontal",
    legend.key.size = unit(1, "lines"),
    legend.text = element_text(size = 12,
                               color = "gray40",
                               face = "bold",
                               family = "sans")
    ) +
  labs(
    title = "The budget surplus from Buckets A, B, and E compensates for\nthe shortfall in Buckets C and D"
  )
