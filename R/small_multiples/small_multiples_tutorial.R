library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)

multiples_data <- data.frame(
  question = c(
    "I hold a leadership position in my school or community.",
    "I have volunteered for a community org in the past 12 mos.",
    "I try to find time to make a difference in my community.",
    "My close friends have volunteered in the past 12 mos.",
    "I see leadership opportunities for Latino/Hispanic youth.",
    "I am working on my leadership skills.",
    "I enjoy doing kind acts for others."
  ),
  yes = c(
    0.2162162,
    0.3421053,
    0.3783784,
    0.4473684,
    0.4736842,
    0.5,
    0.6
  ),
  sorta = c(
    0.3243243,
    0.4473684,
    0.5675676,
    0.3947368,
    0.3947368,
    0.3157895,
    0.35
  ),
  no = c(
    0.4594595,
    0.2105263,
    0.05405405,
    0.1578947,
    0.1315789,
    0.1842105,
    0.05
  )
) %>%
  pivot_longer(2:4, names_to = "response", values_to = "proportion") %>% 
  mutate(response_f = factor(response, levels = c("yes", "sorta", "no")),
                proportion = round(100*proportion),
                label_position = dplyr::if_else(proportion < 15, true = proportion, false = 0),
                label_color = dplyr::if_else(response == "no", true = "black", false = "white"))

## Assign colors to be used for each response category.
## For now, make sure that the preferred colors match their respective responses.
colors <- data.frame(
  facet_values = c("yes", "sorta", "no"),
  colors = c("purple", "blue", "grey")
)

## Merge colors together with the original data, readying us for the graph.
input_data <- left_join(multiples_data, colors, by = c("response" = "facet_values"))

plot <- ggplot(data = input_data, aes(x = question, y = proportion)) +
  geom_bar(aes(fill = colors), stat = "identity", position = "identity", width = 3/4) +
  scale_fill_manual(values = c("purple" = "#3B2883", "blue" = "#8EAEE8", "grey" = "#D8D8D8")) +
  facet_wrap(~response_f) +
  coord_flip()

axis_order <- multiples_data %>%
  filter(response == "yes") %>%
  arrange(proportion) %>%
  select(question)

plot <- plot +
  scale_x_discrete(limits = axis_order$question) +
  ## Stretch the bar graphs to align completely with strip text
  scale_y_continuous(expand = c(0, 0))

plot <- plot +
  theme_minimal() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      face = "bold", 
      hjust = 0, 
      size = 12
    ),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(
      face = "bold",
      size = 10, 
      color = "black",
      margin = margin(
        r = 15
      )
    ),
    axis.text.x = element_blank(),
    panel.spacing = unit(3, "lines"),
    axis.line.x = element_blank(),
    axis.line.y = element_line()
  ) +
  xlab(NULL) + 
  ylab(NULL)

plot <- plot +
  geom_text(
    aes(
      group = response_f, 
      label = paste0(proportion, "%"),
      color = label_color,
      y = label_position,
      hjust = 0
    ),
    position = position_nudge(
      x = 0, 
      y = 2
    )
  ) +
  scale_color_manual(
    values = c(
      "white" = "white", 
      "black" = "black"
    )
  )
