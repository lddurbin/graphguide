library(shiny) # Web Application Framework for R
library(dplyr) # A Grammar of Data Manipulation
library(lubridate) # Make Dealing with Dates a Little Easier
library(purrr) # Functional Programming Tools 
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics

make_heatmap <- function(fill_value) {
  ggplot(data = scrobble_summary) +
    geom_tile(aes(x = day, y = reorder(month, desc(month)), fill := {{fill_value}}), color = "white", size = 1) +
    scale_x_discrete(position = "top") +
    scale_fill_gradient(low="white", high="#1DB954") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_text(size = 12),
      legend.position = "bottom",
      legend.title = element_text(size=0)
    )
}

path <- here::here("R/heatmap")

podcasts <- readRDS(fs::dir_ls(path, glob = "*.rds"))

scrobbles <- map(fs::dir_ls(path = path, glob = "*.json"), jsonlite::fromJSON) %>%
  bind_rows() %>% 
  tibble() %>% 
  filter(msPlayed > 0 & !artistName %in% podcasts) %>% 
  mutate(
    datetime = ymd_hm(endTime) %>% with_tz("Pacific/Auckland"),
    date = as_date(datetime),
    month = month(date, label = TRUE),
    day = wday(date, label = TRUE, week_start = 1),
    .keep = "unused")

scrobble_summary <- scrobbles %>% 
  group_by(day, month) %>% 
  summarise(avg_plays = mean(n()), avg_duration = round(mean(msPlayed)/60), .groups = "drop") %>% 
  filter(month != "Jan")

ui <- fluidPage(
  titlePanel("When did I listen to Spotify in 2021?"),
  radioButtons("plays_or_duration", label = "See number of plays or seconds spent listening?", choices = c("Number of plays", "Seconds spent listening"), width = "110%"),
  plotOutput("plot", width = "816px", height = "785px")
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    if(input$plays_or_duration == "Number of plays") {
      make_heatmap(avg_plays)
    } else {
      make_heatmap(avg_duration)
    }
  })
}

shinyApp(ui, server)
