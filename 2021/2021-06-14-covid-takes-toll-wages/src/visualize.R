# Packages ----

library(conflicted)
library(here)
conflict_prefer("here", "here")
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(lubridate)
library(dfrtheme)
library(patchwork)
library(ggtext)

dirYear <- "2021"
dirProject <- "2021-06-14-covid-takes-toll-wages"

i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Plot ----

## Real wage growth ----

wageGrowth <- read_csv(here(dirYear, dirProject, "result", "wage-growth.csv"))

plotWage <- ggplot(wageGrowth, aes(x = date, y = wage_real_growth)) +
  geom_hline(yintercept = 0, lwd = 12/22, color = "black") +
  geom_line(lwd = 1, color = "#6078A8FF") +
  geom_vline(
    xintercept = ymd("2020-03-01"),
    color = "#757575",
    lwd = 0.5,
    lty = "dashed"
  ) +
  geom_text(
    data = tibble(x = ymd("2020-03-01"), y = 20, label = "COVID-19 pandemic"),
    aes(x = x, y = y, label = label),
    color = "#757575",
    size = dfr_convert_font_size(),
    hjust = 1,
    nudge_x = -25,
    nudge_y = -2.5
  ) +
  scale_x_continuous(
    breaks = seq(ymd("2016-02-01"), ymd("2020-02-01"), by = "1 year"),
    labels = c("Feb\n2016", "'17", "'18", "'19", "'20")
  ) +
  scale_y_continuous(
    breaks = seq(-20, 20, 10),
    limits = c(-20, 20),
    position = "right"
  ) +
  labs(
    subtitle = "Average net wage\\* year-on-year change<br>(percent)",
    x = NULL,
    y = NULL
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "#757575"),
    panel.grid.major.x = element_blank()
  )


## Unemployment rate ----

unemp <- read_csv(here(dirYear, dirProject, "result", "unemployment-rate.csv"))

plotUnemp <- ggplot(unemp, aes(x = date, y = unemployment_rate)) +
  geom_line(lwd = 1, color = "#6078A8FF") +
  geom_vline(
    xintercept = ymd("2020-03-01"),
    color = "#757575",
    lwd = 0.5,
    lty = "dashed"
  ) +
  scale_x_continuous(
    breaks = seq(ymd("2016-02-01"), ymd("2020-02-01"), by = "1 year"),
    labels = c("Feb\n2016", "'17", "'18", "'19", "'20")
  ) +
  scale_y_continuous(
    breaks = seq(0, 8, 2),
    limits = c(0, 8),
    expand = c(0, 0),
    position = "right"
  ) +
  labs(
    subtitle = "Unemployment rate<br>(percent)",
    x = NULL,
    y = NULL
  ) +
  dfr_theme() +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    panel.grid.major.x = element_blank()
  )


## Patchwork: real wage growth and unemployment rate ----

plotWage +
  plotUnemp +
  plot_annotation(
    title = "Pandemic hits wage, employment",
    caption = paste0(
      "\\*At 2010 prices<br>",
      "Source: Statistics Indonesia (BPS); World Bank; ",
      "author's analysis<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    ),
    theme = dfr_theme()
  )

ggsave(
  here(dirYear, dirProject, "result", "wage-unemployment.png"),
  width = 8,
  height = 4.5
)


## Real wage growth and sectoral value added growth ----

wageValueAddedGrowth <- read_csv(
  here(dirYear, dirProject, "result", "wage-value-added-growth.csv")
)

filterSector <- c(
  "Construction",
  "Transportation and warehouse",
  "Accommodation and food and beverage services",
  "Information and communication",
  "Healthcare and social activity services"
)

annotationSector <- wageValueAddedGrowth %>%
  filter(sector %in% filterSector) %>%
  mutate(
    sector = str_replace_all(
      sector,
      c(
        "Transportation and warehouse" =  "Transportation and warehousing",
        "Accommodation and food and beverage services" = "Accommodation and<br>F&B services",
        "Healthcare and social activity services" = "Healthcare and<br>social services",
        "Information and communication" = "Information and<br>communication"
      )
    )
  )


ggplot(
  wageValueAddedGrowth,
  aes(x = value_added_growth, y = wage_real_growth)
) +
  geom_vline(
    xintercept = 0,
    color = "black",
    lwd = 12/22,
    lty = "solid"
  ) +
  geom_hline(
    yintercept = 0,
    color = "black",
    lwd = 12/22,
    lty = "solid"
  ) +
  geom_point(
    pch = 21,
    color = "white",
    fill = "#486090FF",
    size = 4.5,
    alpha = 0.75
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    lwd = 0.75,
    color = "#757575",
    lty = "dashed"
  ) +
  geom_richtext(
    data = annotationSector,
    aes(label = sector),
    size = dfr_convert_font_size(),
    color = "#757575",
    hjust = 0,
    nudge_x = 0.5,
    label.size = 0,
    label.color = NA
  ) +
  scale_x_continuous(breaks = seq(-20, 20, 10), limits = c(-20, 20)) +
  scale_y_continuous(
    breaks = seq(-20, 0, 5),
    limits = c(-20, 0),
    position = "right"
  ) +
  labs(
    title = "Decline in wage follows contraction across all sectors",
    subtitle = paste0(
      "Average net wage\\* and gross value added\\* ",
      "annual change in 2020, by sector (percent)"
    ),
    x = "Gross value added",
    y = "Average net wage",
    caption = paste0(
      "\\*At 2010 prices<br>",
      "Source: Statistics Indonesia (BPS); World Bank; author's analysis<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    )
  ) +
  dfr_theme()

ggsave(
  here(dirYear, dirProject, "result", "wage-value-added-growth.png"),
  width = 8,
  height = 4.5
)


## Nominal wage and minimum wage ----

wageMin <- read_csv(here(dirYear, dirProject, "result", "wage-minimum.csv"))

wageMinClean <- wageMin %>%
  filter(province != "Indonesia") %>%
  mutate(
    across(.cols = c(wage_minimum_2021, wage_nominal), .fns = ~ .x / 1000000),
    province = fct_reorder(province, wage_minimum_2021),
    year = year(date),
    category = case_when(
      year == 2021 & wage_nominal > wage_minimum_2021 ~ "Higher than wage floor",
      year == 2021 & wage_nominal < wage_minimum_2021  ~ "Lower than wage floor"
    )
  ) %>%
  group_by(province) %>%
  fill(category, .direction = "up") %>%
  ungroup()

ggplot(wageMinClean, aes(y = province)) +
  geom_point(
    aes(x = wage_nominal, fill = as.factor(year)),
    pch = 21,
    color = "white",
    size = 3,
    alpha = 0.75
  ) +
  geom_point(
    aes(x = wage_minimum_2021),
    pch = 21,
    color = "white",
    fill = "#A8A8A8FF",
    size = 3,
    alpha = 0.75
  ) +
  geom_text(
    data = tibble(
      x = 2.981379,
      y = "East Kalimantan",
      label = "Wage floor",
      category = "Higher than wage floor"
    ),
    aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    hjust = 1,
    nudge_x = -0.175,
    color = "#A8A8A8FF",
    fontface = "bold"
  ) +
  scale_x_continuous(breaks = seq(1, 5), limits = c(1, 5)) +
  scale_fill_manual(values = c("2020" = "#90A8C0FF", "2021" = "#304890FF")) +
  facet_wrap(~ category, ncol = 2, scales = "free_y") +
  labs(
    title = "Wage falls below minimum level in some provinces",
    subtitle = paste0(
      "Average net wage\\* and minimum wage in 2021 ",
      "by province (million rupiah)"
    ),
    x = NULL,
    y = NULL,
    caption = paste0(
      "\\*February figures<br>",
      "Source: Statistics Indonesia (BPS)<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    )
  ) +
  dfr_theme() +
  theme(
    axis.text.y = element_text(hjust = 0),
    panel.grid.major.y = element_blank(),
    legend.position = c(0.02, 1.025),
    legend.justification = c(0, 1)
  )

ggsave(
  here(dirYear, dirProject, "result", "wage-minimum.png"),
  width = 8,
  height = 5.5
)
