library(dplyr)
library(ggplot2)
library(reactablefmtr)
library(htmltools)

names <- nzbabynames::nzbabynames |> 
  mutate(Name = stringi::stri_conv(Name, from = "latin1", to = "UTF-8"))

last_featured <- names |> 
  group_by(Name) |> 
  filter(Year == max(Year)) |> 
  ungroup() |> 
  select(Name, last_year_featured = Year, last_year_count = Count)

max_year <- names |> 
  group_by(Name) |> 
  filter(Count == max(Count)) |> 
  ungroup() |> 
  select(Name, max_year = Year, max_year_value = Count)

name_stats <- left_join(last_featured, max_year, by = "Name")

names_over_time <- names |> 
  left_join(name_stats, by = "Name") |> 
  with_groups(c(Name, last_year_featured, max_year, max_year_value), summarise, names_data = list(Count))
  
names_table <- reactable(
  names_over_time,
  theme = fivethirtyeight(),
  fullWidth = FALSE,
  showPageInfo = FALSE,
  paginationType = "jump",
  defaultPageSize = 25,
  showPageSizeOptions = TRUE,
  pageSizeOptions = c(25, 50, 75, 100),
  filterable = TRUE,
    columns = list(
      Name = colDef(maxWidth = 200),
      last_year_featured = colDef(
        name = "Last Featured",
        maxWidth = 100,
        filterable = FALSE
        ),
      max_year = colDef(
        name = "Year Peaked",
        maxWidth = 100,
        filterable = FALSE
        ),
      max_year_value = colDef(
        name = "Peak Year Total",
        maxWidth = 100,
        filterable = FALSE
        ),
      names_data = colDef(
        cell = react_sparkline(
          names_over_time,
          highlight_points = highlight_points(max="blue")
          ),
        filterable = FALSE,
        name = "Trend",
        minWidth = 300
      )
    )
  )

save_reactable_test(names_table, here::here("R/sparklines/nzbabynames.html"))
