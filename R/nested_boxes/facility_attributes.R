library(dplyr) # A Grammar of Data Manipulation
library(readxl) # Read Excel Files
library(tidyr) # Tidy Messy Data
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(purrr) # Functional Programming Tools

path <- here::here("R/nested_boxes")


# Functions ---------------------------------------------------------------

filter_and_count <- function(df, column, value, new_column) {
  df |> 
    filter({{column}} == {{value}}) |> 
    with_groups({{column}}, mutate, {{new_column}} := n())
}

define_x <- function(facilities_count, prior_count, value_count) {
  parent_max <- prior_count / facilities_count
  proportion_of_parent <- value_count / prior_count
  x_max <- parent_max * proportion_of_parent
  
  return (x_max * 0.5)
}

define_y <- function(tile_number, tile_margin = 0.15) {
  y_max <- 1 - (tile_number*tile_margin)
  
  return (y_max * 0.5)
}


# Data + chart ------------------------------------------------------------

facilities <- read_excel(fs::dir_ls(path, glob = "*.xlsx"), sheet = 2, skip = 1, .name_repair = janitor::make_clean_names) 

facilities_boxes <- facilities |>
  select(facility_name, governance_model, delivery_model, ownership) |> 
  mutate(facilities_total = n()) |> 
  filter_and_count(governance_model, "Connected Communities", "have_a_Connected_Communities_governance_model") |> 
  filter_and_count(delivery_model, "Community led", "are_community_led") |> 
  filter_and_count(ownership, "Council-owned", "are_Council_owned") |> 
  distinct(across(-c(facility_name:ownership))) |> 
  pivot_longer(everything(), names_to = "category", values_to = "value") |> 
  mutate(prior_response = lag(value), total_facilities = 292, tile_number = row_number()-1) |> 
  tail(3) |> 
  mutate(
    category = stringr::str_replace_all(category, "_", " "),
    x_pos = define_x(total_facilities, prior_response, value),
    x_width = x_pos * 2,
    y_pos = define_y(tile_number),
    y_height = y_pos * 2,
    percent = paste0(round((value/prior_response)*100), "%"),
    facilities_count = paste0("(",value, " out of ", prior_response," facilities", ")")
  )

ggplot() +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1), fill = "#1a5ea9") +
  pmap(
    list(
      facilities_boxes$x_pos,
      facilities_boxes$y_pos,
      facilities_boxes$x_width,
      facilities_boxes$y_height,
      0.08*facilities_boxes$tile_number
    ),
    ~geom_tile(data = facilities_boxes, aes(x = ..1, y = ..2), width = ..3, height = ..4, fill = "#EEE0FF", alpha = ..5)
  ) +
  pmap(
    list(
      c(rep(0.05, 3), 0.33, rep(0.18, 2)),
      rep(facilities_boxes$y_height-0.05, 2),
      c(facilities_boxes$percent, facilities_boxes$category),
      c(rep(9, 3), rep(5, 3))
    ),
    ~annotate("text", x = ..1, y = ..2, label = ..3, size = ..4, colour = "white")
  ) +
  theme_void() +
  theme(title = element_text(size=18)) +
  labs(
    title = "How are our facilities governed, led, and owned?"
  )
