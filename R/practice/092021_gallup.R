library("googlesheets4") # Access Google Sheets using the Sheets API V4
library("dplyr") # A Grammar of Data Manipulation
library("tidyr") # Tidy Messy Data
library("ggplot2") # Create Elegant Data Visualisations Using the Grammar of Graphics
library("ggtext")

# gallup <- read_sheet("https://docs.google.com/spreadsheets/d/13Ks-X6fvf635BDakyYHuot_U6Y9tm4quJ0OgkARM3cc/edit#gid=0")

gallup_cleaned <- gallup %>%
  rename(dimension = 1) %>%
  pivot_longer(2:4, names_to = "date") %>%
  mutate(category = case_when(
    dimension != "Favor" & dimension != "Oppose" ~ dimension,
    dimension == "Favor" | dimension == "Oppose" ~ NA_character_,
  )) %>%
  fill(category) %>%
  filter(!is.na(value) & date != "May 2021") %>%
  pivot_wider(names_from = dimension, values_from = value) %>%
  mutate(net_favourability = (Favor-Oppose)/100, date = factor(date, levels = c("Apr 2021", "Aug 2021")))

gallup_reshaped <- gallup_cleaned %>% 
  select(-c("Favor", "Oppose")) %>% 
  pivot_wider(names_from = date, values_from = net_favourability) %>% 
  janitor::clean_names() %>% 
  mutate(change = aug_2021 - apr_2021)

gallup_dotplot <- gallup_reshaped %>%
  left_join(gallup_cleaned) %>% 
  ggplot(aes(x = reorder(category, change), y = net_favourability, fill = date)) +
  geom_dotplot(binaxis='y', stackdir='center', binwidth = .015) +
  scale_fill_manual(values=c("grey", "blue")) +
  coord_flip() +
  ggthemes::theme_fivethirtyeight() +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    axis.text = element_text(size = 12)
    ) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L)) +
  labs(
    title = "Support among Americans for COVID-19 vaccine certificates<br>increased between <span style='color:grey'>April 2021</span> and <span style='color:blue'>August 2021</span>, especially<br>when it comes to restaurant dining and hotel stays",
    caption = "Net favourability (% favour minus % oppose) of Americans when asked: Would you favor or oppose\nbusinesses requiring people to show proof of coronavirus/COVID-19 vaccination in order to do the\nfollowing over the next several months?"
    ) +
  theme(plot.title = element_markdown(lineheight = 1.1))

# ggsave(here::here("R/practice/gallup_makeover.png"), gallup_dotplot)

gallup_bell <- gallup_reshaped %>% 
  arrange(desc(change)) %>% 
  mutate(category_num = row_number())

Y_AXIS <- seq(from = -.2, to = .25, by = .05)
X_AXIS_LABELS <- gallup_bell$category
X_AXIS_BREAKS <- gallup_bell$category_num

gallup_bell_plot <- gallup_bell %>%
  ggplot() +
  geom_segment(aes(x = reorder(category_num, -change), xend = category_num, y = apr_2021, yend = aug_2021), size = 1, colour = "grey") +
  geom_point(aes(x = reorder(category_num, -change), y = apr_2021), fill = "grey", colour = "grey", size=7) +
  geom_point(aes(x = reorder(category_num, -change), y = aug_2021), fill = "blue", colour = "blue", size=7) +
  geom_label(
    x = 1.15,
    y = -.2,
    label = "April",
    size = 6,
    color = "grey",
    hjust = 0,
    label.padding = unit(0.4, "lines"),
    label.size = 0
    ) +
  geom_label(
    x = 1.15,
    y = .06,
    label = "August",
    size = 6,
    color = "blue",
    hjust = 0,
    label.padding = unit(0.4, "lines"),
    label.size = 0
  ) +
  scale_x_discrete(breaks = X_AXIS_BREAKS, labels = X_AXIS_LABELS) +
  scale_y_continuous(limits = range(Y_AXIS), breaks = Y_AXIS, labels = scales::label_percent(accuracy = 1L)) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    plot.title.position = "plot",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    plot.background = element_rect(fill = "white"),
    panel.background =  element_rect(fill = "white"),
    panel.grid.major.x = element_blank()
  ) +
  # scale_y_continuous(labels = scales::label_percent(accuracy = 1L)) +
  labs(
    title = "Across all four areas they were asked about, Americans were more in favour of vaccine<br>certificates in <span style='color:blue'>August 2021</span> than in <span style='color:grey'>April 2021</span>",
    caption = "Net favourability (% favour minus % oppose) of Americans when asked: Would you favor or oppose\nbusinesses requiring people to show proof of coronavirus/COVID-19 vaccination in order to do the\nfollowing over the next several months?"
  ) +
  theme(plot.title = element_markdown(lineheight = 1.1))

ggsave(here::here("R/practice/gallup_bell.png"), gallup_bell_plot)