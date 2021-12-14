library(dplyr) # A Grammar of Data Manipulation
library(lubridate) # Make Dealing with Dates a Little Easier
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics

df <- readr::read_csv(
  here::here("R/area/vaccines_trends_nz.csv"),
  skip = 2,
  col_types = "Di",
  name_repair = janitor::make_clean_names
)

ggplot(df, aes(x = week, y = vaccine_new_zealand)) +
  geom_area() +
  geom_area(data = filter(df, week >= ymd("2021-08-18") & week < ymd("2021-12-02")), fill = "red", colour = "red") +
  annotate("text", x = ymd("2021-10-10"), y = 15, label = "Auckland Lockdown", size = 5.5, colour = "white") +
  scale_x_date(date_labels = "%b", breaks = "1 month") +
  ggthemes::theme_fivethirtyeight() +
  theme(
    panel.grid.major = element_line(size = 0),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 11)
  ) +
  labs(
    title = "In 2021, New Zealanders were Googling about vaccines a lot as\nAuckland went into lockdown... and again as Auckland opened up",
    caption = "Google trends data in New Zealand for the term \"vaccine\" | Source: https://trends.google.com/trends/explore?q=vaccine&geo=NZ"
  )
