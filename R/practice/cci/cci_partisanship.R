library(dplyr) # A Grammar of Data Manipulation
library(lubridate) # Make Dealing with Dates a Little Easier
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics

pdf_raw <- pdftools::pdf_data(here::here("R/practice/cci/demopoliticalparty202111.pdf")) %>%
  bind_rows(.id = "page")

index_data <- pdf_raw %>% mutate(value_type = case_when(
  y > 550 & page == "3" ~ "ignore",
  y > 122 & x < 100 ~ "Month",
  y > 122 & x < 150 ~ "Year",
  y > 122 & x < 200 ~ "Democrats",
  y > 122 & x < 230 ~ "Independents",
  y > 122 & x < 270 ~ "Republicans"
)) %>% 
  filter(!is.na(value_type) & value_type != "ignore") %>% 
  select(page, y, value = text, value_type) %>% 
  tidyr::pivot_wider(names_from = value_type, values_from = value) %>% 
  mutate(
    date = as_date(paste(Year, "-", Month, "-", "01")),
    across(Democrats:Republicans, as.double),
    partisan_divide = abs(Democrats-Republicans)
    ) %>% 
  select(date, partisan_divide) %>% 
  filter(date >= ymd("2006-01-01"))

ggplot(index_data) +
  geom_area(aes(x = date, y = partisan_divide)) +
  ggthemes::theme_fivethirtyeight() +
  scale_x_date(breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Sharp partisan divides in US consumer sentiment have grown in\nrecent years",
    caption = "Size of partisan gap in the Index of Consumer Sentiment, 2006 to present | Data: https://bit.ly/3Dkvp6M"
    ) +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(size=12)
  )
