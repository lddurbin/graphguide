library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggforce) # Accelerating 'ggplot2'
library(purrr) # Functional Programming Tools
library(magick) # Advanced Graphics and Image-Processing in R

directory <- here::here("R/comets")

physical_checkouts <- readr::read_csv(fs::dir_ls(directory, glob = "*.csv"), col_types = "ccccd", name_repair = janitor::make_clean_names) %>% 
  mutate(date = if_else(financial_year == "FY18", "jan_2018", "jan_2022"), location_name = case_when(
      location_name == "Massey" & date == "jan_2018" ~ "Te Manawa",
      location_name == "Leys Institute" & date == "jan_2018" ~ "Leys Institute Little",
      TRUE ~ location_name)) %>% 
  filter(financial_year %in% c("FY18", "FY22") & month == "Jan" & !local_board %in% c("Mobile Libraries", "Other") & sum_of_checkouts_actual > 0 & location_name != "Takaanini") %>% 
  select(-c(financial_year, month)) %>% 
  tidyr::pivot_wider(names_from = date, values_from = sum_of_checkouts_actual) %>% 
  mutate(colour = if_else(jan_2018 > jan_2022, "decrease", "increase"))

# Make the comet plot

plot <- ggplot(physical_checkouts) +
  geom_link(aes(x = jan_2018, xend = jan_2022, y = reorder(location_name, jan_2022), yend = reorder(location_name, jan_2022), size = stat(index), color = colour), lineend = "round") +
  map(
    c("increase", "decrease"),
    ~geom_point(data = filter(physical_checkouts, colour == .x), aes(x = jan_2022, y = location_name), shape = 21, fill = "white", size = 5, stroke = 0)) +
  map2(
    c("decrease", "increase"), c(1.18, -0.15),
    ~geom_label(data = filter(physical_checkouts, colour == ..1), aes(x = jan_2022, y = location_name, label = location_name), hjust = ..2, fill = "white", label.size = NA)
    ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_line(0.5, colour = "#BEBEBE"),
    plot.title.position = "plot",
    plot.title = element_text(size=16),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.margin = margin(10, 10, 20, 10)
    ) +
  scale_x_continuous(labels = scales::label_comma(), breaks = seq(0, 50000, by = 10000), limits = c(-12000,50000)) +
  scale_color_manual(values = c("increase" = "orange", "decrease" = "#ADD8E6")) +
  xlab("Physical checkouts, January 2018 vs January 2022") +
  labs(
    title = "In January 2022, only one of Auckland's public libraries experienced an\nincrease in physical checkouts compared to four years previously"
  )
 
ggsave(filename = paste0(directory, "/Comet_PNG_version.png"), plot = plot,
       height = 12, width = 8, units = "in", dpi = 300)

# Make an inset plot that we'll use for our key
inset_plot <- ggplot(data = tibble(x = seq(1:10), y = seq(1:10))) +
  geom_link(aes(x = 1, xend = 2, y = 2, yend = 2, size = stat(index)), lineend = "round", colour = "#ADD8E6") +
  geom_point(aes(x = 2, y = 2), shape = 21, fill = "white", size = 4.5, stroke = 0) +
  scale_x_continuous(limits = c(0.7,2.3)) +
  scale_y_continuous(limits = c(1.5,3.2)) +
  pmap(list(c(1.5,1,2), c(3,2.5,2.5), c("Key", "January\n2018", "January\n2022"), c(9,6,6)), ~annotate(geom = "text", x = ..1, y = ..2, label = ..3, size = ..4)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(size=1, fill = 'floralwhite', color = "black")
    )

ggsave(filename = paste0(directory, "/Inset.png"),  inset_plot, w = 4, h = 2, units = "in", dpi = 200, type = "cairo")

inset <- image_read(paste0(directory, "/Inset.png"))
graf <- image_read(paste0(directory, "/Comet_PNG_version.png"))
image_composite(graf, inset, offset = "+1400+3000") %>% image_write(paste0(directory, "/Comet_plot.png"))

