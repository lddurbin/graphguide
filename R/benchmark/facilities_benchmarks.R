library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(dplyr) # A Grammar of Data Manipulation
library(ggtext) # Improved Text Rendering Support for 'ggplot2'

path <- here::here("R/benchmark")

data <- readxl::read_excel(
  path = fs::dir_ls(path, glob = "*.xlsx"),
  sheet = "LTP Report table",
  .name_repair = janitor::make_clean_names
  )

lb_groups <- readr::read_csv(fs::dir_ls(path, glob = "*.csv"))

facilities_fy22 <- data |> 
  left_join(lb_groups, by = c("location" = "local_board")) |> 
  filter(location != "Regional")

labels <- tibble(local_board_region = c("CE", "NW", "S"), strip_title = c("Central-East", "North-West", "South"), x = c(8.15,9.15,6.15))

plot <- ggplot(facilities_fy22, aes(x = reorder(location, desc(location)), y = fy22_actuals)) +
  geom_col(fill = "#1E88E5") +
  geom_col(aes(y = fy21_actuals), fill = "#B2BEB5", width = 0.3) +
  geom_errorbar(aes(ymax=sum_of_target_fy22_from_hao, ymin=sum_of_target_fy22_from_hao), color = "orange", size = 2.5) +
  geom_text(data = labels, aes(x = x, y = 0.4, label = strip_title), size=5.5) +
  scale_x_discrete(expand=c(0.4,0)) +
  scale_y_continuous(expand=c(0.02,0.015), breaks = seq(0,1.0, by = 0.1), labels = scales::percent) +
  coord_flip() +
  facet_grid(local_board_region ~ ., scales = "free", space = "free") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.y = element_text(size=12, margin=margin(r=-4)),
    axis.text.x = element_text(size=12),
    panel.grid = element_blank(),
    strip.text = element_blank(),
    plot.title.position = "plot",
    plot.title = element_markdown(size=20)
  ) +
  labs(title = "How did the <span style='color:#1E88E5'>FY22 actuals</span> in each region's Local Board perform against <span style='color:orange'>**this**<br>year's target</span>, and how do they compare to <span style='color:#B2BEB5'>last year's actuals?</span>")

ggsave(filename = "facilities_benchmarks.png", plot = plot, device = "png", bg = "white", width = 10, height = 12, dpi = 300, path = path)
