library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)

employee_experience <- read_csv(here::here("R/small_multiples/HEDERP_survey.csv")) %>% 
  filter(Question_Type == "Likert" & Question_Grouping == "Your employee experience" & !is.na(Response) & Response != "-99") %>% 
  select(ResponseId, Question_Wording, Response) %>% 
  mutate(Response = case_when(
    Response %in% c("Strongly agree", "Agree") ~ "Agree",
    Response %in% c("Strongly disagree", "Disagree") ~ "Disagree",
    TRUE ~ Response
  )) %>% 
  with_groups(Question_Wording, mutate, responses = n()) %>% 
  with_groups(c(Question_Wording, Response), mutate, proportion = round(n()/responses*100)) %>% 
  distinct(Question_Wording, Response, proportion) %>% 
  mutate(
    Response = factor(Response, levels = c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree")),
    label_position = case_when(
      proportion < 18 ~ proportion,
      TRUE ~ 0
    ),
    colour = case_when(
      Response == "Agree" ~ "purple",
      Response == "Neither agree nor disagree" ~ "blue",
      Response == "Disagree" ~ "grey"
    ),
    label_colour = case_when(
      proportion < 18 | Response == "Disagree" ~ "black",
      TRUE ~ "white"
    ),
    Question_Wording = word(Question_Wording, 11, -1)
  )

axis_order <- employee_experience %>%
  filter(Response == "Agree") %>%
  arrange(proportion) %>%
  select(Question_Wording)

employee_experience %>% 
  ggplot(aes(x = Question_Wording, y = proportion)) +
  geom_bar(aes(fill = colour), stat = "identity", position = "identity", width = 3/4) +
  scale_fill_manual(values = c("purple" = "#3B2883", "blue" = "#8EAEE8", "grey" = "#D8D8D8")) +
  facet_wrap(~Response) +
  coord_flip() +
  scale_x_discrete(limits = axis_order$Question_Wording, labels = function(x) str_wrap(x, width = 18)) +
  geom_text(aes(
    group = Response,
    label = paste0(proportion, "%"),
    color = label_colour,
    y = label_position,
    hjust = 0,
    size = 12
  ),
  position = position_nudge(x = 0, y = 2)) +
  scale_color_manual(values = c("white" = "white", "black" = "black")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    axis.line = element_blank(),
    strip.background = element_blank(),
    panel.grid = element_blank(),
    title = element_text(size = 16),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    panel.spacing = unit(3, "lines"),
    axis.text.y = element_text(size = 14, color = "black", margin = margin(r = 15)),
    strip.text = element_text(size = 14)
  ) +
  labs(
    title = "Whilst higher education development staff enjoy the work\nthat they do, only half of them feel adequately paid",
    subtitle = "Proportion of development professionals working in UK higher education\ninstitutions who responded to each of the following HEDERP survey questions",
    caption = "Data : https://github.com/lddurbin/HEDERP"
    )
