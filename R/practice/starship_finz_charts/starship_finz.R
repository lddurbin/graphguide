library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(readxl) # Read Excel Files
library(tidyr) # Tidy Messy Data
library(ggtext) # Improved Text Rendering Support for 'ggplot2'
library(magick) # Advanced Graphics and Image-Processing in R

path <- here::here("R/practice/starship_finz_charts")

new_vs_existing <- read_excel(path = fs::dir_ls(path, glob = "*.xlsx"), .name_repair = janitor::make_clean_names) |> 
  mutate(
    across(c(-giving_band), as.double),
    across(where(is.double), ~replace_na(.x, 0))
  ) |> 
  filter(giving_band != "Unknown")

new_donors <- new_vs_existing |> 
  select(giving_band, new_donors_percent, new_donors_number) |> 
  mutate(
    variable = "new",
    new_donors_percent_formatted = scales::label_percent(accuracy = 1)(new_donors_percent),
    new_donors_number_formatted = scales::label_comma()(new_donors_number),
    label_new = paste0(new_donors_percent_formatted, " (n=", new_donors_number_formatted, ")"),
    position = 0.095 + nchar(new_donors_number)*0.012
    )

existing_donors <- new_vs_existing |> 
  select(giving_band, existing_donors_percent, existing_donors_number) |> 
  mutate(
    variable = "existing",
    existing_donors_percent_formatted = scales::label_percent(accuracy = 1)(existing_donors_percent),
    existing_donors_percent = existing_donors_percent*-1,
    existing_donors_number_formatted = scales::label_comma()(existing_donors_number),
    label_existing = paste0(existing_donors_percent_formatted, " (n=", existing_donors_number_formatted, ")"),
    position = -0.1 - nchar(existing_donors_number)*0.012
    )

plot <- ggplot() +
  geom_bar(
    data = new_donors, 
    stat = "identity", 
    position = "identity", 
    fill = "#DC0963",
    aes(x = giving_band, 
        y = new_donors_percent)
  ) +
  geom_bar(
    data = existing_donors, 
    stat = "identity", 
    position = "identity", 
    fill = "#4A63AE",
    aes(x = giving_band, 
        y = existing_donors_percent)
  ) +
  coord_flip() +
  scale_x_discrete(limits = new_vs_existing$giving_band) +
  annotate(
    "text",
    x = new_donors$giving_band,
    y = new_donors$position,
    label = new_donors$label_new,
    colour = "white",
    size=4.25,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = existing_donors$giving_band,
    y = existing_donors$position,
    label = existing_donors$label_existing,
    colour = "white",
    size=4.25,
    fontface = "bold"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    axis.text.y = element_text(family = "sans", size = 14),
    plot.title = element_markdown(size=24, margin = margin(0,0,0.75,0, "lines")),
    plot.margin = margin(0.5, 0.5, 2.5, 0.5, "lines")
  ) +
  labs(
    title = "The highest-value contributions to the PICU Campaign came from<br><span style='color:#4A63AE'>existing donors</span>,whereas most of the smaller sums came from <span style='color:#DC0963'>new donors</span>"
  )

ggsave("starship_picu_donors_chart.png", plot, path = path, bg = "white", width = 1700, height = 1800, units = "px", dpi = 150)

starship_plot <- image_read(paste0(path, "/starship_picu_donors_chart.png"))
starship_logo <- image_read(paste0(path, "/starship_logo.png")) |> image_resize("100")

# get dims of the plot
plot_height <- magick::image_info(starship_plot)$height
plot_width <- magick::image_info(starship_plot)$width

# get dims of the logo
logo_width <- magick::image_info(starship_logo)$width
logo_height <- magick::image_info(starship_logo)$height

# get number of pixels to be 1% from the bottom of the plot
# while accounting for the logo height
plot_height - logo_height - plot_height * 0.01

# get number of pixels to be 1% from the left of the plot
plot_width - logo_width - logo_width * 0.01

starship_plot |> 
  image_composite(starship_logo, offset = "+1585+1728") |> 
  image_write(path = paste0(path, "/starship_composite.png"))
