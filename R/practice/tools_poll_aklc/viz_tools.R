library(dplyr)
library(ggplot2)
library(ggtext)

poll <- tibble::tribble(
  ~tool,     ~current, ~future,
  "Excel",    10,       4,
  "Power BI", 6,        8,
  "R",        1,        5,
  "Tableau",  0,        1,
  "Python",   0,        3,
  "Other",    2,        2
) |> 
  mutate(
    tool_number = row_number(),
    offset = case_when(
      current > future ~ 0.2,
      current < future ~ -0.2,
      TRUE ~ 0
    )
  )

plot <- ggplot() +
  geom_segment(
    data = poll,
    aes(x = tool_number, xend = tool_number, y = current, yend = future + offset),
    color = "#778899",
    size = 1.25,
    arrow = arrow(length = unit(0.5, "lines"), ends = "last", type = "closed")
    ) +
  geom_point(
    data = poll,
    aes(x = tool_number, y = current),
    colour = "#ADD8E6", size = 7
  ) +
  geom_point(
    data = poll,
    aes(x = tool_number, y = future),
    colour = "orange", size = 7
  ) +
  geom_label(
    aes(x = rep(1.15, times = 2), y = c(4, 10)),
    label = c("What you'd like to learn", "What you use"),
    hjust = 0,
    size = 4.5,
    label.padding = unit(0.4, "lines"),
    label.size = 0
    ) +
  scale_x_continuous(breaks = poll$tool_number, labels = poll$tool) +
  scale_y_continuous(breaks = c(0, 2,4,6,8,10)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    plot.title.position = "plot",
    plot.title = element_markdown(size=18)
  ) +
    labs(
      title = "Most of us use Excel to make graphs, but we want to learn how to make them in other tools"
    )

ggsave(here::here("R/practice/tools_poll_aklc/poll_results.png"), plot = plot, bg = "white")
