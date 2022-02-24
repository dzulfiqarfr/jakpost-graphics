# Packages ----

library(conflicted)
library(here)
conflict_prefer("here", "here")
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(lubridate)
library(dfrtheme)
library(ggtext)
library(ggrepel)
library(patchwork)

dirYear <- "2021"
dirProject <- "2021-08-12-emergency-curbs-cci-rsi"

i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Plot ----

## Consumer confidence index ----

cci <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "consumer-confidence-index.csv"
  )
)

plotCCI <- ggplot(cci, aes(x = date, y = consumer_confidence_index)) +
  geom_hline(yintercept = 100, lwd = 12/22) +
  geom_line(lwd = 1, color = "#2477B3") +
  geom_richtext(
    data = tibble(
      x = c(ymd("2017-06-01"), ymd("2017-06-01")),
      y = c(110, 90),
      label = c("Optimistic &#8593;", "Pessimistic &#8595;")
    ),
    aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    hjust = 0,
    color = "#757575",
    label.padding = unit(0, "lines"),
    label.size = NA
  ) +
  geom_text(
    data = tibble(x = ymd("2020-03-01"), y = 140, label = "COVID-19 pandemic"),
    aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    hjust = 1,
    nudge_x = -25,
    nudge_y = -5,
    color = "#757575"
  ) +
  geom_text_repel(
    data = tibble(
      x = ymd("2020-05-01"),
      y = 70,
      label = "Large-scale\nsocial restrictions"
    ),
    aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    hjust = 1,
    nudge_x = -175,
    color = "#757575"
  ) +
  geom_text_repel(
    data = tibble(x = ymd("2021-08-01"), y = 130, label = "Emergency\ncurbs"),
    aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    hjust = 1,
    nudge_x = -75,
    color = "#757575"
  ) +
  geom_vline(
    xintercept = ymd("2020-03-01"),
    lwd = 0.5,
    lty = "dashed",
    color = "#757575"
  ) +
  annotate(
    geom = "rect",
    xmin = ymd("2020-04-01"),
    xmax = ymd("2020-05-01"),
    ymin = 40,
    ymax = 140,
    fill = "#757575",
    alpha = 0.25
  ) +
  annotate(
    geom = "rect",
    xmin = ymd("2021-07-01"),
    xmax = ymd("2021-08-01"),
    ymin = 40,
    ymax = 140,
    fill = "#757575",
    alpha = 0.25
  ) +
  scale_x_date(
    breaks = seq(ymd("2017-01-01"), ymd("2021-01-01"), by = "1 year"),
    labels = c("2017", str_c("'", seq(18, 21)))
  ) +
  scale_y_continuous(
    breaks = seq(40, 140, 20),
    limits = c(40, 140),
    position = "right"
  ) +
  labs(
    subtitle = "Consumer confidence index",
    x = NULL,
    y = NULL
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "#E0E0E0"),
    panel.grid.major.x = element_blank()
  )


## Retail sales index ----

rsi <- read_csv(here(dirYear, dirProject, "result", "retail-sales-index.csv"))

plotRSI <- ggplot(rsi, aes(x = date, y = retail_sales_index_growth)) +
  geom_hline(yintercept = 0, lwd = 12/22) +
  geom_line(lwd = 1, color = "#2477B3") +
  scale_x_date(
    breaks = seq(ymd("2017-01-01"), ymd("2021-01-01"), by = "1 year"),
    labels = c("2017", str_c("'", seq(18, 21)))
  ) +
  geom_vline(
    xintercept = ymd("2020-03-01"),
    lwd = 0.5,
    lty = "dashed",
    color = "#757575"
  ) +
  annotate(
    geom = "rect",
    xmin = ymd("2020-04-01"),
    xmax = ymd("2020-05-01"),
    ymin = -45,
    ymax = 30,
    fill = "#757575",
    alpha = 0.25
  ) +
  annotate(
    geom = "rect",
    xmin = ymd("2021-07-01"),
    xmax = ymd("2021-08-01"),
    ymin = -45,
    ymax = 30,
    fill = "#757575",
    alpha = 0.25
  ) +
  scale_y_continuous(
    breaks = seq(-45, 30, 15),
    limits = c(-45, 30),
    position = "right"
  ) +
  labs(
    subtitle = "Retail sales index year-on-year change\\* (percent)",
    x = NULL,
    y = NULL
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "#E0E0E0"),
    panel.grid.major.x = element_blank()
  )


## Patchwork: consumer confidence index, retail sales index ----

plotCCI +
  plotRSI +
  plot_annotation(
    title = paste0(
      "Emergency curbs reverse recovery in consumer confidence, ",
      "retail sales"
    ),
    caption = paste0(
      "\\*Last figure (July 2021) is an estimate<br>",
      "Source: Bank Indonesia (BI)<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    ),
    theme = dfr_theme()
  )

ggsave(
  here(dirYear, dirProject, "result", "consumer-confidence-retail-sales.png"),
  width = 8.5,
  height = 4.75
)


## Mobility to retail and recreation places by region ----

mobilityRegion <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "mobility-retail-region.csv"
  )
)

plotMobilityRegion <- ggplot(
  mobilityRegion,
  aes(x = date, y = mobility_region_7day_moving_average, color = region)
) +
  geom_hline(yintercept = 0, lwd = 12/22) +
  geom_line(lwd = 1, show.legend = FALSE) +

  geom_text_repel(
    data = tibble(
      x = ymd("2020-04-01"),
      y = -35,
      label = "Large-scale\nsocial restrictions"
    ),
    aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    hjust = 0,
    nudge_x = 50,
    color = "#757575"
  ) +
  geom_text_repel(
    data = tibble(x = ymd("2021-08-01"), y = -25, label = "Emergency curbs"),
    aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    hjust = 1,
    nudge_x = -50,
    color = "#757575"
  ) +
  geom_text(
    data = tibble(
      x = c(ymd("2021-05-01"), ymd("2021-04-20")),
      y = c(4, -18.5),
      label = c("Other", "Java & Bali")
    ),
    aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    hjust = 0.5,
    color = c("#55CBF2", "#2477B3"),
    fontface = "bold"
  ) +
  geom_vline(
    xintercept = ymd("2020-03-02"),
    color = "#757575",
    lwd = 0.5,
    lty = "dashed"
  ) +
  annotate(
    geom = "rect",
    xmin = ymd("2020-04-01"),
    xmax = ymd("2020-05-01"),
    ymin = -40,
    ymax = 10,
    fill = "#757575",
    alpha = 0.25
  ) +
  annotate(
    geom = "rect",
    xmin = ymd("2021-07-01"),
    xmax = last(mobilityRegion$date),
    ymin = -40,
    ymax = 10,
    fill = "#757575",
    alpha = 0.25
  ) +
  geom_label(
    data = tibble(x = ymd("2020-03-01"), y = 7.5, label = "COVID-19 pandemic"),
    aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    hjust = 0,
    nudge_x = 5,
    color = "#757575",
    label.padding = unit(0, "lines"),
    label.size = 0
  ) +
  scale_x_date(
    breaks = seq(ymd("2020-03-01"), ymd("2021-08-01"), by = "3 month"),
    labels = c("Mar\n2020", "Jun", "Sep", "Dec", "Mar\n'21", "Jun")
  ) +
  scale_y_continuous(
    breaks = seq(-40, 10, 10),
    limits = c(-40, 10),
    position = "right"
  ) +
  scale_color_manual(
    values = c(
      "Java & Bali" = "#2477B3",
      "Other" = "#55CBF2"
    )
  ) +
  labs(
    subtitle = "Change in visitors by region\\* (percent)",
    x = NULL,
    y = NULL
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "#E0E0E0"),
    panel.grid.major.x = element_blank()
  )


## Mobility and COVID-19 confirmed cases per million, by province ----

mobilityCovid <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "mobility-covid.csv"
  )
)

annotationProvince <- mobilityCovid %>%
  filter(province %in% c("Bali", "Jakarta", "Gorontalo", "Aceh"))

plotMobilityCovid <- ggplot(
  mobilityCovid,
  aes(x = cases_per_million_people, y = mobility_province_7day_moving_average)
) +
  geom_hline(yintercept = 0, lwd = 12/22) +
  geom_point(
    pch = 21,
    fill = "#36A3D9",
    color = "white",
    alpha = 0.75,
    size = 4
  ) +
  geom_smooth(
    method = "lm",
    color = "black",
    se = FALSE,
    lty = "dashed",
    lwd = 0.75
  ) +
  geom_label(
    data = annotationProvince,
    aes(label = province),
    size = dfr_convert_font_size(),
    hjust = 1,
    color = "#757575",
    nudge_x = 0.025,
    nudge_y = 5,
    label.padding = unit(0, "lines"),
    label.size = 0
  ) +
  scale_x_log10(
    breaks = c(4000, 10000, 40000, 100000),
    labels = c(4000, "10,000", "40,000", "100,000"),
    limits = c(4000, 100000)
  ) +
  scale_y_continuous(
    breaks = seq(-50, 25, 25),
    limits = c(-50, 25),
    position = "right"
  ) +
  labs(
    subtitle = "Change in visitors and COVID-19 cases by province\\*\\*",
    x = "Cumulative cases per 1 million people (log scale)",
    y = "Change in visitors (percent)"
  ) +
  dfr_theme()


## Patchwork: mobility and COVID-19 ----

plotMobilityRegion +
  plotMobilityCovid +
  plot_annotation(
    title = "Mobility falls before govt tightens restrictions as cases rise",
    subtitle = paste0(
      "Change in visitors to retail and recreation places ",
      "from January-February 2020 baseline,<br>",
      "seven-day moving average"
    ),
    caption = paste0(
      "\\*Weighted by population ",
      "\\*\\*Latest data available as of Aug. 8, 2021<br>",
      "Source: Google; Statistics Indonesia (BPS); ",
      "COVID-19 task force; author's analysis<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    ),
    theme = dfr_theme()
  )

ggsave(
  here(dirYear, dirProject, "result", "mobility-covid.png"),
  width = 8.75,
  height = 5.25
)
