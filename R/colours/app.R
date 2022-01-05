library(shiny) # Web Application Framework for R
library(dplyr) # A Grammar of Data Manipulation
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(lubridate) # Make Dealing with Dates a Little Easier
library(tidyr) # Tidy Messy Data
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(purrr) # Functional Programming Tools
library(stringr) # Simple, Consistent Wrappers for Common String Operations

colour_block <- function(xmin, xmax, primary, brand_colour) {
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ifelse(primary, 0.725, 0.2), ymax = ifelse(primary, 0.9, 0.37)),  fill = brand_colour, colour = "black")
}

text_block <- function(x_coordinates, primary, text) {
  geom_text(aes(x = x_coordinates, y = ifelse(primary, 0.625, 0.1), label = text))
}

df <- tribble(
  ~x, ~y,
  0, 0,
  1, 1.
)

layout <- tribble(
  ~xmin_coordinates_block, ~xmax_coordinates_block, ~x_coordinates_text, ~colour,   ~block_text,                       ~primary,
  0,                       0.17,                     0.075,             "#007CB9",  "Shore\nCalm, sanctuary",           TRUE,
  0.2075,                  0.3775,                   0.295,             "#00304B",  "Ocean\nExploration, frontier",     TRUE,
  0.415,                   0.585,                    0.51,              "#F8F8F8",  "Off white\nBeing, balance",        TRUE,
  0,                       0.17,                     0.075,             "#6E963C",  "Leaf\nNature, sustainability",     FALSE,
  0.2075,                  0.3775,                   0.295,             "#CC423E",  "Nikau berry\nAllure, prominence",  FALSE,
  0.415,                   0.585,                    0.51,              "#C0D67A",  "Fields\nGrowth, healing",          FALSE,
  0.6225,                  0.7925,                   0.71,              "#87C9DD",  "Sky\nGuidance, vision",            FALSE,
  0.83,                    1,                        0.9,               "#E76317",  "Sunset\nRest, horizon",            FALSE
)

img <- png::readPNG(here::here("R/colours/aklc.png"))
g <- grid::rasterGrob(img, width = .3, interpolate=TRUE)

ui <- fluidPage(
  radioButtons("colour_level", label = "Primary or Secondary Colour Palette?", choices = c(TRUE, FALSE)),
  plotOutput("plot", width = "816px", height = "785px")
)

server <- function(input, output, session) {
  params <- reactive(layout %>% filter(primary == input$colour_level))
  
  output$plot <- renderPlot({
    ggplot(df) +
      annotation_custom(g, xmin=0.21, xmax=0.81, ymin=-0.1, ymax=0.05) +
      {if(input$colour_level == TRUE) geom_text(aes(x = .175, y = 0.95, label = "Primary colour palette"), size = 6, fontface = "bold")} +
      {if(input$colour_level == TRUE) pmap(list(params()$xmin_coordinates_block[1:3], params()$xmax_coordinates_block[1:3], params()$primary[1:3], params()$colour[1:3]), colour_block)} +
      {if(input$colour_level == TRUE) pmap(list(params()$x_coordinates_text[1:3], params()$primary[1:3], params()$block_text[1:3]), text_block)} +
      {if(input$colour_level == FALSE) geom_text(aes(x = .2, y = 0.43, label = "Secondary colour palette"), size = 6, fontface = "bold")} +
      {if(input$colour_level == FALSE) pmap(list(params()$xmin_coordinates_block[1:5], params()$xmax_coordinates_block[1:5], params()$primary[1:5], params()$colour[1:5]), colour_block)} +
      {if(input$colour_level == FALSE) pmap(list(params()$x_coordinates_text, params()$primary[1:5], params()$block_text[1:5]), text_block)} +
      scale_x_continuous(limits = c(0,1)) +
      scale_y_continuous(limits = c(-0.05,1)) +
      ggthemes::theme_fivethirtyeight() +
      theme(
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        title = element_text(family = "Arial", size = 15)
      ) +
      labs(title = "Te Kunihera o Tāmaki Makaurau brand colours\nand their associations")
  })
}

shinyApp(ui, server)
