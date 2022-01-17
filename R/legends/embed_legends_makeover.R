# Load the necessary packages.
library(ggplot2, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(ggtext, warn.conflicts = FALSE)

data <- read_csv(here::here("R/legends/Population_by_Nationality.csv"))

format_data <- data %>% filter(Nationality %in% c("India", "China", "Philippines", "South Africa", "Great Britain"))

label_data <- format_data %>%
  filter(Date == max(Date)) %>% 
  arrange(Nationality) %>% 
  mutate(palette = c("red", "forestgreen", "orange", "blue", "black"), labels = c("China", "Great Britain", "India", "Philippines", "South Africa"))

range <- c(as.Date("2008-07-31"), as.Date("2023-07-01"))

p <- ggplot(data = format_data, aes(x = Date, y = Count, group = Nationality, color = Nationality)) +
  geom_line(size = 2) +
  annotate("text", x = label_data$Date, y = label_data$Count, label = label_data$labels, color = label_data$palette, hjust = -0.2) +
  scale_color_manual(values = label_data$palette) +
  ggthemes::theme_fivethirtyeight() +
  labs(x = "", y = "") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_date(limits = range, breaks = "2 years", labels = scales::label_date("%Y")) +
  labs(
    title = "A decade ago, most migrants to New Zealand came<br>from <span style='color:forestgreen'>Great Britain</span>. These days, it's <span style='color:orange'>India</span>.",
    subtitle = "Number of migrants in New Zealand per month by nationality of Visa application or passport",
    caption = "Data: https://www.immigration.govt.nz/about-us/research-and-statistics/statistics"
  ) +
  theme(plot.title = element_markdown(lineheight = 1.1))

p

ggsave(file = here::here("legends/who_wants_to_be_a_kiwi.png"), dpi = 150)
