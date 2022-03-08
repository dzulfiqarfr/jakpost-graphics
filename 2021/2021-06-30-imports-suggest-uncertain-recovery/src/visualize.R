# Packages ----

library(conflicted)
library(here)
library(tidyverse)
library(lubridate)
library(dfrtheme)
library(patchwork)

dirYear <- "2021"
dirProject <- "2021-06-30-imports-suggest-uncertain-recovery"

i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Plot ----

importContributionChange <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "import-contribution-change-yoy.csv"
  )
)

importContributionChangeSplit <- importContributionChange %>%
  split(.$category_second)


## Raw materials ----

importRawMaterial <- importContributionChangeSplit %>%
  .[["Raw materials"]] %>%
  mutate(
    category_fourth = fct_relevel(
      category_fourth,
      c(
        "Food and beverages",
        "Parts and accessories",
        "Fuels and lubricants",
        "Industrial supplies"
      )
    )
  )

plotImportRawMaterial <- ggplot(
  data = importRawMaterial,
  mapping = aes(x = date, y = contribution_change_category_second)
) +
  geom_col(
    mapping = aes(fill = category_fourth),
    position = "stack",
    color = "white",
    width = 25,
    lwd = 0.25
  ) +
  geom_hline(yintercept = 0, lwd = 12/22) +
  geom_vline(
    xintercept = ymd("2020-03-01"),
    color = "#757575",
    lwd = 0.5,
    lty = "dashed"
  ) +
  geom_text(
    data = tibble(x = ymd("2020-03-01"), y = 40, label = "COVID-19 pandemic"),
    mapping = aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    color = "#757575",
    hjust = 1,
    nudge_x = -15,
    nudge_y = -5
  ) +
  scale_x_date(
    breaks = seq(ymd("2018-01-01"), ymd("2021-01-01"), "1 year"),
    label = c("Jan\n2018", "'19", "'20", "'21")
  ) +
  scale_y_continuous(
    breaks = seq(-60, 40, 20),
    limits = c(-60, 40),
    position = "right"
  ) +
  scale_fill_manual(
    values = c(
      "Food and beverages" = "#90A4AE",
      "Parts and accessories" = "#39D7E6",
      "Fuels and lubricants" = "#1D97BF",
      "Industrial supplies" = "#266099"
    )
  ) +
  labs(
    subtitle = "Raw materials",
    x = NULL,
    y = NULL
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "#E0E0E0"),
    legend.justification = c(0, 0),
    legend.position = c(0.025, 0.1),
    legend.key.size = unit(0.75, "lines"),
    legend.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank()
  )


## Capital goods ----

importCapitalGoods <- importContributionChangeSplit %>%
  .[["Capital goods"]] %>%
  mutate(
    category_fourth = str_replace_all(
      category_fourth,
      c(
        "Capital goods \\(except transport equipment\\)" = "Capital goods excluding transport equipment",
        "Passenger motor cars" = "Passenger cars",
        "Other transport equipment, industrial" = "Other transport equipment for industries"
      )
    ),
    category_fourth = fct_relevel(
      category_fourth,
      c(
        "Passenger cars",
        "Other transport equipment for industries",
        "Capital goods excluding transport equipment"
      )
    )
  )

plotImportCapitalGoods <- ggplot(
  data = importCapitalGoods,
  mapping = aes(x = date, y = contribution_change_category_second)
) +
  geom_col(
    mapping = aes(fill = category_fourth),
    position = "stack",
    color = "white",
    width = 25,
    lwd = 0.25
  ) +
  geom_hline(yintercept = 0, lwd = 12/22) +
  geom_vline(
    xintercept = ymd("2020-03-01"),
    color = "#757575",
    lwd = 0.5,
    lty = "dashed"
  ) +
  scale_x_date(
    breaks = seq(ymd("2018-01-01"), ymd("2021-01-01"), "1 year"),
    label = c("Jan\n2018", "'19", "'20", "'21")
  ) +
  scale_y_continuous(
    breaks = seq(-75, 50, 25),
    limits = c(-75, 50),
    position = "right"
  ) +
  scale_fill_manual(
    values = c(
      "Passenger cars" = "#1FE0C0",
      "Other transport equipment for industries" = "#18AD95",
      "Capital goods excluding transport equipment" = "#0E6153"
    )
  ) +
  labs(
    subtitle = "Capital goods",
    x = NULL,
    y = NULL
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "#E0E0E0"),
    legend.justification = c(0, 0),
    legend.position = c(0.025, 0.1),
    legend.key.size = unit(0.75, "lines"),
    legend.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank()
  )


## Patchwork: raw materials and capital goods ----

plotImportRawMaterial +
  plotImportCapitalGoods +
  plot_annotation(
    title = "Imports suggest uncertain recovery",
    subtitle = paste0(
      "Contribution to year-on-year change in imports by category ",
      "(percentage points)"
    ),
    caption = paste0(
      "Source: Bank Indonesia (BI); author's analysis<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    ),
    theme = dfr_theme()
  )

ggsave(
  here(dirYear, dirProject, "result", "import-contribution-change.svg"),
  width = 8,
  height = 4.5
)
