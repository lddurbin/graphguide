library("googlesheets4") # Access Google Sheets using the Sheets API V4
library("dplyr") # A Grammar of Data Manipulation
library("tidyr") # Tidy Messy Data
library("ggplot2") # Create Elegant Data Visualisations Using the Grammar of Graphics
library("ggtext")

gallup <- read_sheet("https://docs.google.com/spreadsheets/d/13Ks-X6fvf635BDakyYHuot_U6Y9tm4quJ0OgkARM3cc/edit#gid=0")

gallup_reshaped <- gallup %>%
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

gallup_reshaped <- gallup_reshaped %>% 
  select(-c("Favor", "Oppose")) %>% 
  pivot_wider(names_from = date, values_from = net_favourability) %>% 
  janitor::clean_names() %>% 
  mutate(change = aug_2021 - apr_2021, .keep = "unused") %>% 
  left_join(gallup_reshaped)

gallup_dotplot <- gallup_reshaped %>%
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

ggsave(here::here("R/practice/gallup_makeover.png"), gallup_dotplot)
