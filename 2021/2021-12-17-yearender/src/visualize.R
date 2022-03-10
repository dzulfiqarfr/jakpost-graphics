# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(ggrepel)
library(dfrtheme)
library(patchwork)

dirYear <- "2021"
dirProject <- "2021-12-17-yearender"

i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Plot ----

covid <- read_csv(here(dirYear, dirProject, "result", "covid-cases-vax.csv"))

covidCase <- covid %>%
  mutate(new_cases_smoothed = new_cases_smoothed / 1000) %>%
  select(-people_fully_vaccinated_per_hundred)

annotate_delta_wave <- function(plot,
                                y_min = 0,
                                y_max = 50) {
  ggplot2::annotate(
    geom = "rect",
    xmin = as.Date("2021-07-01"),
    xmax = as.Date("2021-09-01"),
    ymin = y_min,
    ymax = y_max,
    fill = "#757575",
    alpha = 0.1
  )
}

# Get color palette:
#> library(paletteer)
#> paletteer_d("nord::moose_pond")

## Cases ----

plotCovidCase <- ggplot(
  data = covidCase,
  mapping = aes(x = date, y = new_cases_smoothed)
) +
  geom_line(lwd = 1, color = "#486090FF") +
  geom_text_repel(
    data = tibble(x = as.Date("2021-07-01"), y = 47.5, label = "Delta wave"),
    mapping = aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    color = "#757575",
    hjust = 1,
    nudge_x = -75
  ) +
  annotate_delta_wave() +
  scale_x_date(
    breaks = seq(
      as.Date("2020-01-01"),
      as.Date("2021-07-01"),
      "6 month"
    ),
    labels = c("Jan\n2020", "Jul\n2020", "Jan\n21", "Jul")
  ) +
  scale_y_continuous(
    breaks = seq(0, 50, 10),
    limits = c(0, 50),
    expand = c(0, 0),
    position = "right"
  ) +
  labs(
    title = "Delta cases surge...",
    subtitle = "New confirmed cases, smoothed\\*<br>(thousand)",
    x = NULL,
    y = NULL
  ) +
  dfr_theme() +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    panel.grid.major.x = element_blank()
  )


## Vaccination ----

covidVax <- covid %>%
  filter(!is.na(people_fully_vaccinated_per_hundred)) %>%
  select(-new_cases_smoothed)

plotCovidVax <- ggplot(
  data = covidVax,
  mapping = aes(x = date, y = people_fully_vaccinated_per_hundred)
) +
  geom_line(lwd = 1, color = "#7890A8FF") +
  annotate_delta_wave() +
  scale_x_date(
    breaks = seq(
      as.Date("2021-01-01"),
      as.Date("2021-10-01"),
      "3 month"
    ),
    labels = c("Jan\n21", "Apr\n21", "Jul", "Oct")
  ) +
  scale_y_continuous(
    breaks = seq(0, 50, 10),
    limits = c(0, 50),
    expand = c(0, 0),
    position = "right"
  ) +
  labs(
    title = "amid low vaccination...",
    subtitle = "Share of people fully vaccinated<br>(percent)",
    x = NULL,
    y = NULL
  ) +
  dfr_theme() +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    panel.grid.major.x = element_blank()
  )


## Mobility ----

mobility <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "mobility-retail-transit-workplace.csv"
  )
)

mobilityLong <- mobility %>%
  pivot_longer(
    cols = ends_with("smoothed"),
    names_to = "category",
    values_to = "mobility_smoothed"
  ) %>%
  mutate(
    category = str_remove_all(category, "_smoothed"),
    category = str_replace_all(
      category,
      c(
        "transit" = "Transit stations",
        "retail" = "Retail and recreation",
        "workplace" = "Workplaces"
      )
    )
  ) %>%
  filter(!is.na(mobility_smoothed))

plotMobility <- ggplot(
  data = mobilityLong,
  mapping = aes(x = date, y = mobility_smoothed)
) +
  geom_hline(yintercept = 0, lwd = 12/22) +
  geom_line(aes(color = category), lwd = 1) +
  annotate_delta_wave(y_min = -80, y_max = 20) +
  scale_x_date(
    breaks = seq(
      as.Date("2020-01-01"),
      as.Date("2021-10-01"),
      "6 month"
    ),
    labels = c("Jan\n20", "Jul\n20", "Jan\n21", "Jul")
  ) +
  scale_y_continuous(
    breaks = seq(-80, 20, 20),
    limits = c(-80, 20),
    position = "right"
  ) +
  scale_color_manual(
    values = c(
      "Retail and recreation" =  "#7D4B32FF",
      "Workplaces" = "#AF7D32FF",
      "Transit stations" = "#E1AF4BFF"
    )
  ) +
  labs(
    title = "leading to stricter curbs...",
    subtitle = "Change in mobility\\*\\*, smoothed\\*<br>(percent)",
    x = NULL,
    y = NULL
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "#E0E0E0"),
    legend.position = c(0.1, 0.8),
    legend.justification = c(0, 0),
    legend.key.height = unit(0.25, "lines"),
    legend.key.width = unit(1, "lines"),
    panel.grid.major.x = element_blank()
  )


## Economic growth ----

growth <- read_csv(here(dirYear, dirProject, "result", "gdp-growth.csv"))

plotGrowth <- ggplot(
  data = growth,
  mapping = aes(x = date, y = growth)
) +
  geom_col(width = 60, fill = "#90A8C0FF", color = "white") +
  geom_hline(yintercept = 0, lwd = 12/22) +
  annotate_delta_wave(y_min = -8, y_max = 8) +
  scale_x_date(
    breaks = seq(
      as.Date("2016-01-01"),
      as.Date("2021-01-01"),
      "1 year"
    ),
    labels = c("Q1\n16", seq(17, 21))
  ) +
  scale_y_continuous(
    breaks = seq(-8, 8, 4),
    limits = c(-8, 8),
    position = "right"
  ) +
  labs(
    title = "slowing down recovery",
    subtitle = "Annual GDP growth<br>(percent)",
    x = NULL,
    y = NULL
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "#E0E0E0"),
    panel.grid.major.x = element_blank()
  )


## Patchwork ----

plotCovidCase +
  plotCovidVax +
  plotMobility +
  plotGrowth +
  plot_annotation(
    title = "How the Delta wave disrupted Indonesia's economic recovery",
    caption = paste0(
      "Note: last updated on Dec. 10, 2021 ",
      "\\*Seven-day moving average ",
      "\\*\\*Compared with normal visitors in January-February 2020<br>",
      "Source: Our World in Data; Google; Statistics Indonesia (BPS); ",
      "author's analysis<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    ),
    theme = dfr_theme()
  ) +
  plot_layout(nrow = 1)

ggsave(
  here(dirYear, dirProject, "result", "delta-wave-impact.svg"),
  width = 11,
  height = 5.25
)
