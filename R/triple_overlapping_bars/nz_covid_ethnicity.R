library(dplyr)
library(ggplot2)
library(janitor)
library(ggtext)

# https://www.health.govt.nz/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-case-demographics, as of 4 September
data <- tibble::tribble(
  ~"Prioritised ethnicity", 	~"Total cases", 	~"Hospitalisations for COVID-19", 	~"ICU care for COVID-19",
"Māori", 	                 266146, 	         2468, 	                             105,
"Pacific Peoples", 	       151210, 	         1818, 	                             80,
"Asian", 	                 249345, 	         1153, 	                             34,
"MELAA", 	                 31989, 	         244, 	                             5,
"European or Other", 	     1037094, 	       7516, 	                             263,
"Unknown", 	               11955, 	         29, 	                               2
) |> 
  clean_names() |> 
  adorn_percentages(denominator = "col") |> 
  tibble() |> 
  mutate(prioritised_ethnicity = factor(prioritised_ethnicity, levels = c("Māori", "Pacific Peoples", "Asian", "MELAA", "European or Other", "Unknown")))

plot <- ggplot(data, aes(x = forcats::fct_rev(prioritised_ethnicity))) +
  geom_bar(
    aes(y = total_cases, fill = "Cases"),
    stat = "identity", position = "identity",
    width = 4/5
  ) +
  geom_bar(
    aes(y = hospitalisations_for_covid_19, fill = "Hospitalisation"),
    stat = "identity", position = "identity",
    width = 3/5
  ) +
  geom_bar(
    aes(y = icu_care_for_covid_19, fill = "ICU"),
    stat = "identity", position = "identity",
    width = 2/5
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Cases" = "#c2c4cb",
      "Hospitalisation" = "#29c2e1",
      "ICU" = "#15369e"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 0.65),
    breaks = seq(from = 0, to = 0.65, by = 0.1),
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(size=18),
    legend.position = "none",
    axis.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size=12),
  ) +
  labs(
    title = "Māori and Pacific Peoples are the only ethnic groups who made up a higher proportion<br>of <span style='color:#29c2e1'>hospitalisations</span> and <span style='color:#15369e'>ICU admissions</span> for COVID-19 than of <span style='color:#c2c4cb'>COVID-19 cases</span>",
    subtitle = "Share of COVID-19 cases, hospitalisations, and ICU admissions per ethnic group as of 4 September 2022.\nPrioritised ethnicity classification system as used by the NZ Ministry of Health. MELAA refers to Middle Eastern, Latin American and African peoples.",
    caption = "Source: https://www.health.govt.nz/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-case-demographics"
  )

ggsave(here::here("R/triple_overlapping_bars/nz_covid_ethnicity.png"), plot = plot, device = "png", bg = "#f4f5eb", width = 10.5, height = 8, dpi = 200)
