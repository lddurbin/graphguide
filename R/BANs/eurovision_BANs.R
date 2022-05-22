library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(pins) # Pin, Discover and Share Resources
library(lubridate) # Make Dealing with Dates a Little Easier

path <- here::here("R/BANs")

my_data <- board_url(c("eurovision" = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv"))

eurovision_df <- my_data |> 
  pin_download("eurovision") |> 
  readr::read_csv(show_col_types = FALSE) |> 
  tibble::as_tibble() |> 
  filter(section == "grand-final")

eurovision_2022 <- eurovision_df |> 
  arrange(artist_country, desc(year)) |> 
  with_groups(artist_country, mutate, yoy_perc_2022 = (total_points-lead(total_points))/lead(total_points)) |> 
  filter(year == 2022) |> 
  select(artist_country, points_2022 = total_points, yoy_perc_2022)

eurovision <- eurovision_df |> 
  left_join(eurovision_2022) |> 
  mutate(
    year = lubridate::as_date(paste0(year, "01", "01")),
    earliest_year = round((interval(min(year), max(year)) / years(1))/2,0),
    x_pos = min(year)+years(earliest_year),
    y_pos = max(total_points, na.rm = TRUE)*1.5,
    yoy_perc_2022_formatted = scales::label_percent(accuracy = 1, big.mark = ",")(yoy_perc_2022),
    yoy_perc_2022_formatted = if_else(yoy_perc_2022_formatted == Inf, "", paste0("YoY: ", yoy_perc_2022_formatted))
  ) |> 
  filter(!is.na(total_points) & !is.na(points_2022))

ggplot(eurovision) +
  theme_void() +
  geom_area(
    aes(x = year, y = total_points), 
    group = 1, 
    fill = "gray"
  ) +
  facet_wrap(vars(artist_country)) +
  geom_text(
    aes(x = x_pos, y = y_pos, label = points_2022), 
    size = 8, vjust = 1
  ) +
  geom_text(
    aes(
      x = x_pos,
      y = y_pos*0.7,
      label = yoy_perc_2022_formatted,
      colour = if_else(yoy_perc_2022 < 0, "deepskyblue", "red")),
    size = 5,
  ) +
  labs(
    title = "Scorecards for the Eurovision 2022 Finalists",
    caption = "Points awarded to each country who competed in the 2022 Eurovision Grand Final"
  ) +
  theme(
    strip.text = element_text(size=12),
    legend.position = "none",
    plot.title = element_text(size=24, margin = margin(0.5,0,0.5,0.5, unit = "cm"))
  )
