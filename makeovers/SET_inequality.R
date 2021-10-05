library(dplyr, warn.conflicts = FALSE) # A Grammar of Data Manipulation
library(ggplot2, warn.conflicts = FALSE) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(stringr, warn.conflicts = FALSE) # Simple, Consistent Wrappers for Common String Operations
library(ggtext, warn.conflicts = FALSE) 
library(showtext) # Using Fonts More Easily in R Graphs

data <- readxl::read_excel(here::here("makeovers/SET_equality.xlsx")) %>%
  janitor::clean_names() %>% 
  filter(set == 1 & gender == "Female" & !str_starts(subject, "Anatomy") & (year == 2016 | year == 2011)) %>% 
  mutate(measure = str_to_lower(measure), subject = case_when(
    str_starts(subject, "Psychology") ~ "Psychology, behavioural sciences",
    str_starts(subject, "Pharmacy") ~ "Pharmacy, pharmacology",
    str_starts(subject, "Nursing") ~ "Nursing, allied health studies",
    str_starts(subject, "Mineral") ~ "Mineral, metallurgy, materials engineering",
    str_starts(subject, "Mechanical") ~ "Mechanical, aero, production engineer",
    str_starts(subject, "IT") ~ "IT, systems sciences, computer software engineering",
    str_starts(subject, "Geography") ~ "Geography, environmental studies",
    str_starts(subject, "Electrical") ~ "Eletrical, electronic, computer engineering",
    str_starts(subject, "Earth") ~ "Earth, marine, environmental studies",
    str_starts(subject, "Architecture") ~ "Architecture, built environment, planning",
    str_starts(subject, "Agriculture") ~ "Agriculture, forestry, food science",
    TRUE ~ subject
  )) %>% 
  tidyr::pivot_wider(names_from = measure, values_from = value) %>% 
  select(-c(set, gender, volume))

stem_average <- data %>% 
  with_groups(year, summarise, share = mean(share)) %>% 
  mutate(subject = "STEM subjects average")

font_add_google("Source Sans Pro", "sourcesans")

data %>% 
  bind_rows(stem_average) %>% 
  mutate(year = factor(year, levels = c("2011", "2016"))) %>% 
  ggplot(mapping = aes(x = reorder(subject, share), y = share, fill = year)) +
  geom_dotplot(binaxis='y', stackdir='center', binwidth = .02, show.legend = FALSE) +
  scale_fill_manual(values=c("#56B4E9", "#D55E00")) +
  geom_hline(yintercept = .5) +
  coord_flip() +
  ggthemes::theme_fivethirtyeight(base_size = 11, base_family = "sourcesans") +
  theme(legend.title = element_blank()) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L), breaks = c(.25, .5, .75)) +
  labs(
    title = "In <span style='color:#D55E00'>2016</span>, as in <span style='color:#56B4E9'>2011</span>, women at UK universities<br>were a minority among academic staff in all<br>but four science-based subjects",
    subtitle = "Share of UK academic staff whose legal sex is female (as declared to\nHM Revenue and Customs) for each subject classified as\nSET (Science, Technology, Engineering)",
    caption = "Data: https://www.advance-he.ac.uk/knowledge-hub/tags/reports"
  ) +
  theme(plot.title = element_markdown(lineheight = 1.1))



