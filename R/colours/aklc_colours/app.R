library(shiny) # Web Application Framework for R
library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(purrr) # Functional Programming Tools

colour_block <- function(xmin, xmax, brand_colour) {
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0.725, ymax = 0.9),  fill = brand_colour, colour = "black")
}

text_block <- function(x_coordinates, text) {
  geom_text(aes(x = x_coordinates, y = 0.625, label = text), family = "Arial", size = 5, vjust = -0.5)
}

grid <- tribble(~x, ~y, 0, 0, 1, 1)
layout <- readr::read_csv("brand_data.csv", col_types = "dddccc")

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "journal"),
  radioButtons("colour_level", label = "Primary or Secondary Colour Palette?", choices = c("Primary colour palette", "Secondary colour palette")),
  plotOutput("plot", width = "816px", height = "785px")
)

server <- function(input, output, session) {
  params <- reactive(layout %>% filter(palette == input$colour_level))
  
  img <- png::readPNG("aklc.png")
  g <- grid::rasterGrob(img, width = .3, interpolate=TRUE)
  
  output$plot <- renderPlot({
    ggplot(grid) +
      annotation_custom(g, xmin=0.21, xmax=0.81, ymin= 0.4, ymax= 0.5) +
      geom_text(aes(x = .175, y = 0.95, label = input$colour_level), size = 6, fontface = "bold") +
      pmap(list(params()$xmin_coordinates_block, params()$xmax_coordinates_block, params()$colour), colour_block) +
      pmap(list(params()$x_coordinates_text, params()$block_text), text_block) +
      scale_x_continuous(limits = c(0,1)) +
      scale_y_continuous(limits = c(0,1)) +
      ggthemes::theme_fivethirtyeight() +
      theme(
        axis.text = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        title = element_text(family = "Arial", size = 15)
      ) +
      labs(title = "Te Kunihera o Tāmaki Makaurau brand colours\nand their associations")
  })
}

shinyApp(ui, server)
