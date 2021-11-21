library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(readxl) # Read Rectangular Text Data
library(ggtext)

dogs <- read_excel(here::here("R/slopes/Dog-control-statistics-2021-pivot-table.xlsx"), sheet = "raw data") %>% 
  janitor::clean_names()

dog_changes <- dogs %>% 
  filter(
    dog_control_statistics_category == "B_Registered pure breed",
    !subcategory %in% c( "aa_all (pure)", "zz_other (pure)"),
    item == "1001_total",
    fy_ending_30_june %in% c(2016, 2021),
    council_name == "Auckland (Group)"
    ) %>% 
  tidyr::pivot_wider(names_from = fy_ending_30_june, values_from = value, names_prefix = "fy_") %>% 
  filter(fy_2016 > 0 & fy_2021 > 0) %>%
  mutate(
    fy_2019_total = sum(fy_2016),
    fy_2021_total = sum(fy_2021),
    fy_2019_perc = fy_2016/fy_2019_total,
    fy_2021_perc = fy_2021/fy_2021_total
    ) %>% 
  with_groups(subcategory, mutate, change = (fy_2021_perc - fy_2019_perc), abs_change = abs(change)) %>% 
  mutate(line_colour = case_when(
    change < 0 ~ "red",
    TRUE ~ "blue"
  )) %>% 
  arrange(desc(change))

ggplot(dog_changes) +
  geom_segment(aes(x = 0, y = fy_2019_perc, xend = 1, yend = fy_2021_perc, colour = line_colour), size = 2) +
  geom_point(aes(x = 0, y = fy_2019_perc, colour = line_colour), size = 3) +
  geom_point(aes(x = 1, y = fy_2021_perc, colour = line_colour), size = 3) +
  facet_wrap(~factor(subcategory, levels = unique(dog_changes$subcategory))) +
  geom_text(aes(x = -0.3, y = fy_2019_perc, label = scales::percent(fy_2019_perc, accuracy = 0.1))) +
  geom_text(aes(x = 1.3, y = fy_2021_perc, label = scales::percent(fy_2021_perc, accuracy = 0.1))) +
  labs(
    title = "Compared to five years ago, which registered dog breeds in Auckland became <span style='color:blue'>more popular</span><br>and which became <span style='color:red'>less popular</span> in 2021?",
    caption = "Dog breed registrations in Auckland as a proportion of all dogs registered in Auckland, year ending 30 June 2016 vs year ending 30 June 2021 | Data: https://bit.ly/3Fzk8Bb"
  ) +
  scale_colour_manual(values = c("blue" = "blue", "red" = "red")) +
  ggthemes::theme_fivethirtyeight() +
  scale_x_continuous(breaks = c(0,1), limits = c(-0.5, 1.5), labels = c("2016", "2021")) +
  theme(
    axis.text.y = element_blank(),
    plot.title = element_markdown(lineheight = 1.1),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    axis.line = element_blank(),
    strip.text = element_text(size = 12, vjust = -0.8)
  )
