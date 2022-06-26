library(googlesheets4) # Access Google Sheets using the Sheets API V4
library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(RColorBrewer) # ColorBrewer Palettes
library(ggtext) # Improved Text Rendering Support for 'ggplot2'

path <- here::here("R/bump_chart")

# data <- read_sheet("https://docs.google.com/spreadsheets/d/1c5B9pqOfLwrClwOmmU9wImlDHI029dG4TOQaVt_rC94/edit#gid=0")
# saveRDS(data, paste0(path, "/pies.rds"))

pies <- readRDS(fs::dir_ls(path, glob = "*.rds")) |> 
  janitor::clean_names()

ranked_districts <- pies |> 
  count(year, district, name = "value") |> 
  arrange(year, desc(value)) |> 
  with_groups(year, mutate, rank = row_number()) |> 
  mutate(
    line_colour = if_else(district == "Hawke's Bay", "orange", "grey"),
    district_colour = if_else(district == "Hawke's Bay", "orange", "black")
    )

plot <- ggplot(ranked_districts, aes(x = year, y = rank, group = district, label = rank, colour = line_colour)) +
  geom_line(size = 1.5) +
  geom_point(size = 9) +
  geom_text(color = "white", fontface = "bold", size = 4) +
  scale_y_reverse(limits=c(8,1)) +
  scale_x_continuous(position = "top", limits = c(2010.5,2021), breaks = seq(2012,2022, by = 1)) +
  scale_color_manual(values = c("#BEBEBE", "orange")) +
  annotate("text", x = rep(2010.75, 10), y = 1:10, label = ranked_districts$district[1:10], hjust=0.5, colour = ranked_districts$district_colour[1:10], size = 5) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 16),
    plot.title.position = "plot",
    plot.title = element_markdown(size=20, lineheight = 1.1),
    plot.subtitle = element_markdown(margin=margin(0,0,20,0), size = 14),
    plot.caption = element_markdown(size=10)
  ) +
  labs(
    title = "The fall and rise of <span style='color:orange'><strong>Hawke's Bay</strong></span> pies",
    subtitle = "After slipping down the rankings, bakeries in Hawke's Bay are once again churning out some of the finest pies in Aotearoa",
    caption = "New Zealand's districts ranked by number of award-winning pies"
  )

ggsave(filename = "pies_ranked.png", plot = plot, device = "png", path = path, bg = "white")
