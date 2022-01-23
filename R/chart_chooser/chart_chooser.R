library(shiny) # Web Application Framework for R
library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(purrr) # Functional Programming Tools

grid <- tribble(~x, ~y, 0, 0, 1, 1)

ui <- fluidPage(
  titlePanel("Quantitative Chart Chooser Decision Tree"),
  
  radioButtons("category_num", label = "How many categories?", choices = c("1", "2", "3+")),
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)