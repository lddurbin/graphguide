library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(stringr) # Simple, Consistent Wrappers for Common String Operations
library(ggtext) # Improved Text Rendering Support for 'ggplot2'

# read a CSV file, clean the column names
read_data <- function(filepath) {
  readr::read_csv(filepath, name_repair = janitor::make_clean_names, show_col_types = FALSE)
}

# prepare the data for ggplot
prep_data <- function(dataframe) {
  max_min <- get_max_min(dataframe)
  
  get_latest(dataframe) |> 
    left_join(max_min) |> 
    mutate(
      position = 1:nrow(max_min), across(where(is.double), ~./100),
      name = str_replace_all(name, "_", " ") |> str_to_sentence()
      )
}

# get the upper bound and lower bound of the data
get_max_min <- function(df) {
  max_min <- df |> 
    tidyr::pivot_longer(cols = -date) |> 
    group_by(name) |> 
    summarise(upper_bound = max(value), lower_bound = min(value))
}

# pull out the latest numbers from the data
get_latest <- function(df) {
  df |> 
    slice_head() |> 
    tidyr::pivot_longer(cols = -date, values_to = "latest_value") |> 
    select(-date)
}

path <- here::here("R/dot_and_bar")

dataframes <- purrr::map(
  fs::dir_ls(path),
  read_data
)

poll_1 <- prep_data(dataframes[[1]])
poll_2 <- prep_data(dataframes[[2]])

data_to_viz <- poll_1

ggplot(data_to_viz) +
  geom_segment(aes(x = position, xend = position, y = lower_bound, yend = upper_bound), size = 7, color = "gray65") +
  geom_point(aes(x = position, y = latest_value), size = 11.5, colour = "blue") +
  geom_text(aes(x = position, y = latest_value, label = scales::label_percent()(latest_value)), colour = "white") +
  scale_y_continuous(limits=c(0,1), breaks = c(0,.5,1), labels = scales::percent_format()) +
  scale_x_continuous(labels = data_to_viz$name) +
  coord_flip() +
  theme_void() +
  theme(
    axis.text = element_text(size=14, hjust=1),
    panel.grid.major.x = element_line(size = 0.5, colour="light grey"),
    plot.title.position = "plot",
    plot.title = element_markdown(size=20),
    plot.margin = margin(0.2,0.2,0.2,0.5, unit="cm")
  ) +
  labs(
    title = "Support in <span style='color:blue'>2022</span> among Americans for a woman's right to choose is the highest it's<br>ever been, and opinions about abortion haven't varied much <span style='color:gray65'>over the decades</span>"
  )

