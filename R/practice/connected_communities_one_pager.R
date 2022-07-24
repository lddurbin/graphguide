library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggtext) # Improved Text Rendering Support for 'ggplot2'
library(ggimage) # Use Image in 'ggplot2'


# Functions ---------------------------------------------------------------

make_pictograph <- function(data, img_size, colours, filename, title_text) {
  plot <- ggplot(data, aes(x = x, y = y, colour = category)) +
    geom_image(aes(image = image), size =img_size) +
    scale_colour_manual(values = colours) +
    coord_flip() +
    scale_y_reverse() +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_markdown(size=28, lineheight = 1.1)
    ) +
    labs(
      title = title_text
    )
  
  ggsave(plot = plot, filename = here::here(paste0("R/practice/", filename)), device = "png", bg = "#FAF9F6") 
}

make_slope <- function(data, filename, title_text, annotation) {
  plot <- ggplot(data) +
    geom_segment(aes(x = 0, y = fy_2021, xend = 1, yend = fy_2022), size = 5, colour = "#FF8C00") +
    geom_point(aes(x = 0, y = fy_2021), size = 10, colour = "black") +
    geom_point(aes(x = 0, y = fy_2021), size = 8.5, colour = "#FF8C00") +
    geom_point(aes(x = 1, y = fy_2022), size = 10, colour = "black") +
    geom_point(aes(x = 1, y = fy_2022), size = 8.5, colour = "#FF8C00") +
    scale_x_continuous(limits = c(-0.2, 1.2), breaks = c(0,1), labels = c("FY 2021", "FY 2022")) +
    scale_y_continuous(limits = c(0,data$fy_2021)) +
    geom_text(
      aes(
        x = -0.1,
        y = fy_2021,
        label = scales::label_number(scale_cut = cut_short_scale(), accuracy = 0.1)(fy_2021)
      ),
      size = 12,
      colour = "black"
    ) +
    geom_text(
      aes(
        x = 1.1,
        y = fy_2022,
        label = scales::label_number(scale_cut = cut_short_scale(), accuracy = 0.1)(fy_2022)
      ),
      size = 12,
      colour = "black"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_markdown(lineheight = 1.1, size=38),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour="grey"),
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size=24)
    ) +
    labs(
      title = title_text
    )
  
  ggsave(plot = plot, filename = here::here(paste0("R/practice/", filename)), device = "png", bg = "#FAF9F6") 
}


# Community Places --------------------------------------------------------

# Buildings

community_places_buildings <- tibble(
  x = c(rep(1:15, 16)),
  y = c(sort(rep(1:16, 15)))
) |> 
  arrange(desc(x), desc(y)) |> 
  mutate(
    image = here::here("R/practice/icon_building.png"),
    position = 240-(row_number()-1),
    category = case_when(
      position < 5 ~ "blank",
      position <= 108 ~ "Council-led",
      TRUE ~ "community-led"
    )
  )

make_pictograph(
  data = community_places_buildings,
  img_size = 0.065,
  colours = c("blank" = "#FAF9F6", "community-led" = "#FF8C00", "Council-led" = "grey"),
  filename = "community_places_buildings.png",
  title_text = "We supported 235 community places,<br>and <span style='color:#FF8C00'>56%</span> of them are <span style='color:#FF8C00'>community led</span>"
)

# Trend

community_places_trend <- tibble(
  metric = "participation",
  fy_2021 = 4500000,
  fy_2022 = 3200000
)

make_slope(
  data = community_places_trend,
  filename = "community_places_trend.png",
  title_text = "Across these sites, participation was 3.2 million this year.<br>That's a drop of 42% from last year, however..."
)

# Satisfaction

community_places_satisfaction <- tibble(
  x = c(rep(1:10, 10)),
  y = c(sort(rep(1:10, 10)))
) |> 
  arrange(desc(x), desc(y)) |> 
  mutate(
    image = here::here("R/practice/heart_icon.png"),
    position = 100-(row_number()-1),
    category = if_else(position <= 17, "not_satisfied", "satisfied")
  )

make_pictograph(
  data = community_places_satisfaction,
  img_size = 0.1,
  colours = c("not_satisfied" = "grey", "satisfied" = "#FF8C00"),
  filename = "community_places_hearts.png",
  title_text = "...<span style='color:#FF8C00'>83%</span> of our community hires told<br>us they were <span style='color:#FF8C00'>satisfied</span>"
)


# Libraries ---------------------------------------------------------------

# Buildings

libraries_buildings <- tibble(
  x = c(rep(1:10, 6)),
  y = c(sort(rep(1:6, 10)))
) |> 
  arrange(desc(x), desc(y)) |> 
  mutate(
    image = here::here("R/practice/icon_building.png"),
    position = 60-(row_number()-1),
    category = case_when(
      position < 4 ~ "blank",
      position > 53 ~ "hubs",
      TRUE ~ "libraries"
    )
  )

make_pictograph(
  data = libraries_buildings,
  img_size = 0.105,
  colours = c("blank" = "#FAF9F6", "libraries" = "#6495ED", "hubs" = "#FF8C00"),
  filename = "libraries_buildings.png",
  title_text = "We have 50 <span style='color:#6495ED'>standalone community<br>libraries</span>, and 7 others which form part<br>of a <span style='color:#FF8C00'>Community Hub</span>"
)

# Trend

libraries_trend <- tibble(
  metric = "participation",
  fy_2021 = 6300000,
  fy_2022 = 4400000
)

make_slope(
  data = libraries_trend,
  filename = "libraries_trend.png",
  title_text = "Across these libraries, we saw 4.4 million visits this year.<br>That's a drop of 42% from last year, however..."
)

# Satisfaction

libraries_satisfaction <- tibble(
  x = c(rep(1:10, 10)),
  y = c(sort(rep(1:10, 10)))
) |> 
  arrange(desc(x), desc(y)) |> 
  mutate(
    image = here::here("R/practice/heart_icon.png"),
    position = 100-(row_number()-1),
    category = if_else(position <= 5, "not_satisfied", "satisfied")
  )

make_pictograph(
  data = libraries_satisfaction,
  img_size = 0.1,
  colours = c("not_satisfied" = "grey", "satisfied" = "orange"),
  filename = "libraries_hearts.png",
  title_text = "...<span style='color:#FF8C00'>95%</span> of library members told<br>us they're <span style='color:#FF8C00'>satisfied</span> with our services"
)
