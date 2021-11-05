library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)

data <- readRDS(here::here("R/overlapping_bars/joined_data.rds"))

local_boards <- read_csv(here::here("R/overlapping_bars/local_board_groups.csv"), col_select = c(1,3), col_types = "cc")

p_and_e <- data %>% 
  filter(as.Date(delivery_datetime) < ymd("2021-08-01") & !is.na(in_which_local_board_was_the_session_delivered)) %>% 
  select(id, local_board = in_which_local_board_was_the_session_delivered, external_agents) %>% 
  count(local_board, external_agents) %>% 
  mutate(external_agents = case_when(
    external_agents ~ "external_agents",
    !external_agents ~ "staff_only"
  )) %>% 
  pivot_wider(names_from = external_agents, values_from = n) %>% 
  mutate(total = staff_only + external_agents) %>% 
  left_join(local_boards, by = "local_board") %>% 
  mutate(
    local_board_region = case_when(
      local_board_region == "NW" ~ "North & West",
      local_board_region == "CE" ~ "Central & East",
      local_board_region == "S" ~ "South"
    ),
    local_board_region = factor(local_board_region, levels = c("North & West", "Central & East", "South")),
    perc = (external_agents/total),
    label_distance = case_when(
      total < 50 | (total < 100 & perc > .3) ~ total,
      TRUE ~ external_agents
      ),
    label_colour = case_when(
      total < 50 | (total < 100 & perc > .3) ~ "black",
      TRUE ~ "white"
    )
    )

p_and_e %>% 
  ggplot() +
  geom_col(aes(x = reorder(local_board, total), y = total, fill = "All Events"), width = 3/4) +
  geom_col(aes(x = reorder(local_board, total), y = external_agents, fill = "Community-led Events"), width = 1/2) +
  geom_text(aes(label = scales::percent(perc, accuracy = 1), x = reorder(local_board, perc), y = label_distance), colour = p_and_e$label_colour, hjust = -0.3) +
  facet_wrap(~local_board_region, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("All Events" = "dark grey", "Community-led Events" = "blue")) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    plot.title.position = "plot",
    strip.text = element_text(size = 12, face = "bold"),
    title = element_text(size = 15, face = "bold", color = "#373737"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(size=1),
    axis.text = element_text(size = 12, face = "bold"),
    legend.position = "top",
    legend.justification = 1,
    legend.title = element_blank(),
    legend.direction = "horizontal",
    legend.key.size = unit(1, "lines"),
    legend.text = element_text(size = 12,
                               color = "gray40",
                               face = "bold",
                               family = "sans")
  ) +
  labs(
    title = "Even Local Boards hosting large numbers of events are\ncollaborating heavily with members of the community"
  )  
