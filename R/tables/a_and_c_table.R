library(dplyr) # A Grammar of Data Manipulation
library(reactablefmtr) # Streamlined Table Styling and Formatting for Reactable
library(htmlwidgets) # HTML Widgets for R

a_and_c <- tibble::tribble(
  ~category,                     ~facilities, ~type,           ~programmes,   ~participants,   ~maori_aspirations,   ~community_led,
  "Partners",                      8,         "",               463,           85529,           .07,                  1,
  "Community-led facilities",      32,        "",               4890,          45552,           .11,                  1,
  "Council-led facilities",        10,       "Council-led (65%):",   579,           500093,          .15,                 .35,
  "Council-led facilities",         0,       "Community-led (35%):", 312,           500093,          .15,                 .35,
  "LDI Community Arts Programmes", 12,        "",               126,           NA,              .33,                  1
)

rtable <- a_and_c %>%
  select(
    Category = category,
    "How many?" = facilities,
    type = type,
    Programmes = programmes,
    "% Māori aspirations" = maori_aspirations,
    -community_led
  ) %>%
  reactable(
    theme = fivethirtyeight(centered = TRUE),
    defaultColDef = colDef(align = 'center'),
    
    columns = list(
      Category = colDef(
        align = "left",
        vAlign = "center",
        name = "",
        style = JS("function(rowInfo, column, state) {
        const firstSorted = state.sorted[0]
        if (!firstSorted || firstSorted.id === 'Category') {
          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.values['Category'] === prevRow['Category']) {
            return { visibility: 'hidden' }
          }
        }
      }")
      ),
      
      'How many?' = colDef(
        style = JS("function(rowInfo, column, state) {
        const firstSorted = state.sorted[0]
        if (!firstSorted || firstSorted.id === 'Category') {
          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.values['Category'] === prevRow['Category']) {
            return { visibility: 'hidden' }
          }
        }
      }"),
        cell = bubble_grid(
          data = .,
          colors = "#084C61",
          min_value = 0,
          max_value = 32
        )
      ),
      
      type = colDef(
        align = "right",
        name = "",
        maxWidth = 150,
        style = list(fontWeight = "bold")
      ),
      
      Programmes = colDef(
        cell = data_bars(
          data = .,
          fill_color = c("#87C9DD", "#007CB9"),
          fill_gradient = TRUE,
          background = 'transparent',
          text_position = 'outside-end',
          number_fmt = scales::comma
        ),
        align = "left",
        minWidth = 180
      ),
      
      '% Māori aspirations' = colDef(
        style = JS("function(rowInfo, column, state) {
        const firstSorted = state.sorted[0]
        if (!firstSorted || firstSorted.id === 'Category') {
          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.values['Category'] === prevRow['Category']) {
            return { visibility: 'hidden' }
          }
        }
      }"),
        align = 'center',
        minWidth = 100,
        cell = data_bars(
          data = .,
          fill_color = '#EEEEEE',
          number_fmt = scales::percent,
          text_position = 'outside-end',
          max_value = 1,
          icon = 'circle',
          icon_color = 'black',
          icon_size = 15,
          text_color = 'black',
          round_edges = TRUE
        )
      )
    )
  )

html_file <- here::here("R/tables/table.html")
img_file <- here::here("R/tables/table.png")
saveWidget(widget = rtable, file = html_file, selfcontained = TRUE)
webshot2::webshot(url = html_file, file = img_file, delay = 0.1)
