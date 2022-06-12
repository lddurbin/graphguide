library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(treemapify) # Draw Treemaps in 'ggplot2'
library(ggtext) # Improved Text Rendering Support for 'ggplot2'

path <- here::here("R/treemaps")

data <- readr::read_csv(fs::dir_ls(path, glob = "*.csv"))

visits <- data |> 
  mutate(fill_colour = if_else(library == "Central City", "#f07f55", "#92b5f1")) |> 
  with_groups(fill_colour, summarise, total_visits = sum(may_2022_visitors)) |> 
  mutate(
    library = if_else(fill_colour == "#f07f55", "Central City Library", "All 8 other Waitematā libraries"),
    box_label = paste0(library, ": ", format(total_visits, big.mark = ","))
    )

plot <- ggplot(visits, aes(area = total_visits, fill = fill_colour)) +
  geom_treemap(layout = "srow", start = "topleft", color = "white") +
  geom_treemap_text(
    color = "white", size = 22,
    label = visits$box_label,
    layout = "srow", start = "topleft"
  ) +
  scale_fill_manual(limits = visits$fill_colour, values = visits$fill_colour) +
  theme(
    legend.position = "none",
    plot.title = element_markdown(lineheight = 1.1, size = 28)
  ) +
  labs(
    title = "<span style = 'color:#f07f55'><strong>Auckland Central City Library</strong></span> welcomed more visitors in<br>May 2022 than <span style = 'color:#92b5f1'><strong>the surrounding eight public libraries</strong></span> combined"
  )

ggsave(plot = plot, file = paste0(path, "/treemap.png"), dpi = 150)