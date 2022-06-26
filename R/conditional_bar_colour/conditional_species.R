library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggimage) # Use Image in 'ggplot2'

path <- here::here("R/conditional_bar_colour")

species_loss <- readxl::read_excel(path = fs::dir_ls(path, glob = "*.xlsx"), .name_repair = janitor::make_clean_names) |> 
  mutate(
    colour = if_else(decreasing_populations >= .5, "red", "blue"),
    image_path = paste0(path, "/", stringr::str_to_lower(taxonomic_group), "_eyes.jpg")
    )

plot <- ggplot(data = species_loss) +
  geom_bar(
    aes(x = decreasing_populations, y = reorder(taxonomic_group, decreasing_populations)),
    stat = "identity",
    position = "identity",
    fill = species_loss$colour
  ) +
  geom_text(aes(x = decreasing_populations, y = taxonomic_group, label = scales::label_percent()(decreasing_populations)),
            hjust = 1.1, fontface = "bold", color = "white", size = 12) +
  geom_image(
    aes(x = .04275, y = taxonomic_group, image = image_path),
    size = 0.1725, by = "height") +
  scale_x_continuous(limits = c(-0.075,.6)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.title = element_text(size=32),
    plot.caption = element_text(size=14),
    axis.text.y = element_text(size = 22, hjust = 0.5),
    plot.margin = margin(0.75, 0.75, 0.75, 0.75, "cm")
  ) +
  labs(
    title = "Since 1970, at least half of the amphibian and fish populations\nhave been in decline",
    caption = "Source: OurWorldinData.org | MakeOver Monday 2021, Week 21"
  )

ggsave(paste0(path, "/species_deline.png"), plot = plot, device = "png", bg = "white")