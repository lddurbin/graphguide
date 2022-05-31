library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggridges) # not installed on this machine
library(pins) # Pin, Discover and Share Resources
library(tidyr) # Tidy Messy Data
library(purrr) # Functional Programming Tools

path <- here::here("R/ridge_plots")

curves <- function(x_start, x_end, y_start, y_end, curve) {
  annotate(
    geom = "curve", x = x_start, y = y_start, xend = x_end, yend = y_end, 
    curvature = curve, arrow = arrow(length = unit(2, "mm")), size = 1
  )
}

labels <- function(x, y, label) {
  annotate(geom = "text", x = x, y = y, label = label, hjust = "left", size = 3.5, fontface = "bold")
}


my_data <- board_url(c("rugby" = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/fifteens.csv"))

womens_fifteen <- my_data |> 
  pin_download("rugby") |> 
  readr::read_csv(show_col_types = FALSE) |> 
  filter(tournament == "World Cup")

team_scores <- womens_fifteen |> 
  select(team = team_1, score = score_1) |> 
  bind_rows(womens_fifteen |> select(team = team_2, score = score_2))

teams <- team_scores |> 
  distinct(team)

score_distribution <- team_scores |> 
  mutate(count = max(team_scores$score)+1) |> 
  distinct(team, count) |> 
  uncount(count) |> 
  mutate(
    score = rep(seq(min(team_scores$score):max(team_scores$score)), nrow(teams))
    )

team_total <- team_scores |> 
  with_groups(team, summarise, world_cup_total = sum(score))

top_teams <- team_total |> 
  top_n(10, wt = world_cup_total) |> 
  pull(team)

score_tally <- team_scores |> 
  left_join(team_total) |> 
  count(team, score, world_cup_total, name = "value")

score_tally_expanded <- left_join(score_distribution, score_tally) |> 
  replace_na(list(value = 0)) |> 
  with_groups(team, ~fill(.x, world_cup_total, .direction = "updown")) |> 
  filter(team %in% top_teams) |> 
  mutate(value = value+0.05)

nz_scores <- score_tally |> filter(team == "New Zealand") |> pull(score)

plot <- ggplot(
  score_tally_expanded, aes(x = score, y = reorder(team, world_cup_total), height = value, fill = team)) + 
  geom_density_ridges(stat = "identity", color = "black", size=0.2, alpha = 0.75) +
  pmap(
    list(max(nz_scores)-7, max(nz_scores)-1, 10.65, 10.35, .15),
    curves
    ) +
  pmap(
    list(max(nz_scores)-32, 10.8, "NZ beat Germany 134-6\nat the 1998 World Cup"),
    labels
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(size=22),
    axis.text = element_text(size=12),
    panel.grid.minor = element_blank()
    ) +
  labs(
    title = "The New Zealand Women's Rugby Team have scored more points than\nany other nation at World Cup tournaments",
    caption = "Distribution of World Cup match scores for top 10 nations by total points scored in non-qualifying World Cup matches | Source: TidyTuesday 2022 [Week 22]"
  )

ggsave("rugby_ridges.png", plot = plot, device = "png", path = path, bg = "white", width = 10, height = 10)
