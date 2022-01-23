library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(lubridate)
library(ggtext)
library(purrr)

curves <- function(x_start, x_end, y_start, y_end) {
  annotate(
    geom = "curve", x = dmy(x_start), y = y_start, xend = dmy(x_end), yend = y_end, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  )
}

labels <- function(x, y, label) {
  annotate(geom = "text", x = dmy(x), y = y, label = label, hjust = "left")
}

physical_visits <- readxl::read_excel(here::here("R/chart_chooser/Physical Visits.xlsx"), skip = 1, .name_repair = janitor::make_clean_names)

tidy_visits <- physical_visits %>% 
  mutate(
    year = as.double(stringr::str_remove(financial_year_short_name_hierarchy_financial_year, "FY")),
    year = ifelse(financial_year_short_name_hierarchy_quarter %in% c("Q3", "Q4"), year+2000, year+1999),
    date = dmy(paste("01", financial_year_short_name_hierarchy_month, year, sep = "-"))
    ) %>% 
  select(date, last_year, current_year) %>% 
  filter(date >= ymd("2021-01-01"))

x_start <- list("01-03-2021", "01-06-2021", "10-07-2021", "01-07-2021", "22-11-2021")
x_end <- list("25-03-2021", "10-05-2021", "28-07-2021", "26-07-2021", "08-11-2021")
y_start <- list(85000, 40000, 265000, 440000, 150000)
y_end <- list(1000, 140000, 290000, 405000, 180000)

label_x <- list("10-02-2021", "01-05-2021", "27-05-2021", "01-06-2021", "18-10-2021")
label_y <- list(100000, 28000, 265000, 475000, 140000)
label_text <- list("Alert Level 4", "Alert Levels 3 & 2", "Alert Level 3", "Delta outbreak\nlockdown begins", "Libraries re-open")

ggplot(data = tidy_visits, aes(x = date)) +
  geom_segment(aes(xend = date, y = current_year, yend = last_year), size = 3/4) +
  geom_point(aes(y = current_year), colour = "blue", size = 6) +
  geom_point(aes(y = last_year), colour = "grey", size = 6) +
  scale_x_date(date_labels = "%b", breaks = "months") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size=12),
    title = element_text(size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "The largest disparities in visits to Auckland Libraries between <span style='color:grey'><strong>2020</strong></span> and <span style='color:blue'><strong>2021</strong></span><br>occured when a COVID-19 lockdown was imposed in one year but not the other",
    caption = "Number of door counts across each of Auckland's 56 libraries and community hubs"
  ) +
  theme(plot.title = element_markdown(lineheight = 1.1), plot.title.position = "plot") +
  pmap(list(x_start, x_end, y_start, y_end), curves) +
  pmap(list(label_x, label_y, label_text), labels)
