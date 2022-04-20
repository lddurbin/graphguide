library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(stringr) # Simple, Consistent Wrappers for Common String Operations

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
  geom_point(aes(x = num, y = Eighteen_to_thirty_four), size = 15, colour = "blue") +
  geom_point(aes(x = num, y = Thirty_five_to_fifty_four), size = 15, colour = "black") +
  geom_point(aes(x = num, y = Fifty_five_plus), size = 15, colour = "green") +
  annotate(geom = "text", x = 1, y = .5575, label = "M", size = 6, colour = "white") +
  annotate(geom = "text", x = 1, y = .4978, label = "B", size = 6, colour = "white") +
  annotate(geom = "text", x = 1, y = .4858, label = "X", size = 6, colour = "white") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.48, .56)) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 21),
    plot.title.position = "plot"
  ) +
  labs(
    title = "A higher proportion of Millennials are more likely to have at least some savings compared\nto Gen Xers and Boomers",
    caption = "Share of Americans with savings | Source: Makeover Monday [https://tinyurl.com/yrmtp3ms]"
  )
