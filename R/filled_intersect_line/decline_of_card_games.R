library(tidyverse) # A Grammar of Data Manipulation
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(tidytext)
library(lubridate)

# tuesdata <- tidytuesdayR::tt_load('2022-01-25')

intersect_date <- function(intersect, current_date) {
  days <- round(365-(intersect*365))
  tibble(date = current_date - period(days, units = "days"))
}

curves <- function(x_start, x_end, y_start, y_end, curve) {
  annotate(
    geom = "curve", x = dmy(x_start), y = y_start, xend = dmy(x_end), yend = y_end, 
    curvature = curve, arrow = arrow(length = unit(2, "mm")), size = 1
  )
}

labels <- function(x, y, label) {
  annotate(geom = "text", x = dmy(x), y = y, label = label, hjust = "left", size = 4.5)
}

details <- readRDS(here::here("R/filled_intersect_line/boardgame_details.rds"))
ratings <- readRDS(here::here("R/filled_intersect_line/boardgame_ratings.rds"))

categories <- details %>% 
  mutate(boardgamecategory = str_sub(boardgamecategory, start = 2, end = -2)) %>% 
  unnest_tokens(category, boardgamecategory, token = stringr::str_split, pattern = ",", drop = FALSE) %>% 
  mutate(category = ifelse(str_detect(category, "card"), "card game", str_sub(str_trim(category), 2, -2)))

cardgames <- categories %>% 
  filter(category == "card game") %>%
  distinct(id, .keep_all = TRUE) %>% 
  count(yearpublished) %>% 
  filter(yearpublished >= 1959 & yearpublished < 2022) %>% 
  mutate(
    date = dmy(paste0("01-01-", yearpublished)),
    diff = n - lag(n),
    change_direction = ifelse(diff*lag(diff) >= 0, "same", "switch"),
    intersect = ifelse(change_direction == "switch", abs((0-lag(diff))/abs(diff-lag(diff))), 0),
    positive_max = ifelse(diff >= 0, diff, 0),
    negative_max = ifelse(diff <= 0, diff, 0)
    ) %>% 
  filter(yearpublished != 1959) %>% 
  select(date, diff, change_direction, positive_max, negative_max, intersect)

new_rows <- cardgames %>% filter(change_direction == "switch")

new_dates <- map2_dfr(new_rows$intersect, new_rows$date, intersect_date)

final_data <- tibble(diff = rep(0,nrow(new_rows)), change_direction = rep("switch", nrow(new_rows)), positive_max = 0, negative_max = 0, intersect = 0) %>% 
  bind_cols(new_dates) %>% 
  bind_rows(cardgames) %>% 
  arrange(date)

x_start <- c("01-01-1986", "01-01-2009", "01-01-2014")
x_end <- c("01-06-1989", "01-06-2013", "01-06-2019")
y_start <- c(30, 80, -130)
y_end <- c(15, 100, -150)
curve = c(-.3, -.3, .3)

label_x <- list("01-01-1981", "01-01-1997", "01-01-2001")
label_y <- list(40, 70, -115)
label_text <- list("The 90s saw the beginning\nof the card game boom", "Card game releases\npeaked in 2014.", "2020 saw the sharpest\ndecrease in the number of\nnew card game releases")
  
ggplot(final_data, aes(x = date)) +
  geom_line(aes(y = diff), size = 3) +
  geom_ribbon(aes(ymin = 0, ymax = positive_max), fill = "#00c3e3") +
  geom_ribbon(aes(ymin = 0, ymax = negative_max), fill = "#ff726f") +
  pmap(list(x_start, x_end, y_start, y_end, curve), curves) +
  pmap(list(label_x, label_y, label_text), labels) +
  geom_hline(yintercept = 0, colour = "#888888") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 12),
    title = element_text(size = 15),
    plot.title.position = "plot"
  ) +
  labs(
    title = "The annual growth in card game releases peaked in 2014. Has the rise\nof digital free-to-play alternatives had an affect on the industry since then?",
    caption = "Annual change in number of card game releases, 1961-2021 | Source: TidyTueday 2022 [Week 4]"
  )
