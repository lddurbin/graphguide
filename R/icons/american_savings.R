library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(stringr) # Simple, Consistent Wrappers for Common String Operations
library(ggtext) # Improved Text Rendering Support for 'ggplot2'

data <- readr::read_csv("r/icons/disaggregated_data.csv")

american_savings <- data %>% 
  mutate(
    age_category = case_when(
      str_detect(category, "Millennials") ~ "Eighteen_to_thirty_four",
      str_detect(category, "Gen Xer") ~ "Thirty_five_to_fifty_four",
      TRUE ~ "Fifty_five_plus"
    ),
    savings_category = case_when(
    !subcategory %in% c("I don't have a savings account", "$0") ~ "Savings",
    TRUE ~ subcategory
  )) %>% 
  with_groups(c(savings_category, category, age_category), summarise, sum_value = sum(value)) %>%
  with_groups(c(savings_category, age_category), summarise, avg_value = mean(sum_value)) %>% 
  filter(savings_category == "Savings") %>% 
  tidyr::pivot_wider(names_from = age_category, values_from = avg_value) %>% 
  mutate(num = 1)

ggplot(data = american_savings) +
  geom_segment(aes(x = num, xend = num, y = Eighteen_to_thirty_four, yend = Thirty_five_to_fifty_four), size = 1, color = "grey") +
  map2(
    c(american_savings$Eighteen_to_thirty_four, american_savings$Thirty_five_to_fifty_four, american_savings$Fifty_five_plus), c("blue", "black", "green3"),
    ~geom_point(aes(x = num, y = ..1), size = 15, colour = ..2)
    ) +
  map2(
    c(.5575, .4978, .4858),
    c("M", "B", "X"),
    ~annotate(geom = "text", x = 1, y = ..1, label = ..2, size = 6, colour = "white")
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.45, .6)) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_markdown(size = 21),
    plot.title.position = "plot"
  ) +
  labs(
    title = "A higher proportion of <strong>Millennials</strong> are more likely to have at least some savings compared<br>to <strong>Gen Xers</strong> and <strong>Boomers</strong>",
    caption = "Share of Americans with savings | Source: Makeover Monday [https://tinyurl.com/yrmtp3ms]"
  )
