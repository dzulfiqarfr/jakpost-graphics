# Packages ----

library(conflicted)
library(here)
conflict_prefer("here", "here")
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(dfrtheme)
library(patchwork)
library(gghighlight)
library(ggrepel)

dirYear <- "2021"
dirProject <- "2021-06-28-homeownership-slides"

i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Plot ----

## Homeownership ----

ownershipIndex <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "homeownership-index.csv"
  )
)


### Index

add_label <- function(df, textColor) {
  ggplot2::geom_text(
    data = df,
    ggplot2::aes(x = x, y = y, label = label),
    color = textColor,
    size = dfrtheme::dfr_convert_font_size(),
    hjust = 1
  )
}

plotOwnershipIndex <- ggplot(
  ownershipIndex,
  aes(x = year, y = homeownership_index)
) +
  geom_hline(yintercept = 100, lwd = 12/22, color = "black") +
  geom_line(aes(color = province), lwd = 1, show.legend = FALSE) +
  add_label(
    df = tibble(x = 2020, y = 90, label = "National"),
    textColor = "#E65639"
  ) +
  add_label(
    df = tibble(x = 2019, y = 115, label = "Bengkulu"),
    textColor = "#127DB3"
  ) +
  add_label(
    df = tibble(x = 2016, y = 82.5, label = "Bali"),
    textColor = "#3DB4CC"
  ) +
  add_label(
    df = tibble(x = 2018, y = 70.5, label = "Jakarta"),
    textColor = "#4CCDD9"
  ) +
  geom_text_repel(
    data = tibble(label = "Jambi", x = 2014, y = 101.37470),
    aes(x, y, label = label),
    size = dfr_convert_font_size(),
    color = "#309BBF",
    hjust = 1,
    nudge_x = -1,
    nudge_y = 7.5,
    segment.curvature = -0.25,
    segment.ncp = 3,
    segment.color = "#309BBF"
  ) +
  geom_text_repel(
    data = tibble(label = "Other provinces", x = 2010, y = 95),
    aes(x, y, label = label),
    size = dfr_convert_font_size(),
    color = "#757575",
    hjust = 1,
    nudge_x = -2,
    nudge_y = 15,
    segment.curvature = -0.25,
    segment.ncp = 3
  ) +
  gghighlight(
    province %in% c("Indonesia", "Jakarta", "Bali", "Bengkulu", "Jambi"),
    label_key = F,
    unhighlighted_params = list(color = "grey", alpha = 0.25)
  ) +
  scale_x_continuous(
    breaks = seq(1999, 2020, 3),
    labels = c("1999", "2002", "'05", "'08", str_c("'", seq(11, 20, 3)))
  ) +
  scale_y_continuous(
    breaks = seq(40, 120, 20),
    limits = c(40, 120),
    position = "right"
  ) +
  scale_color_manual(
    values = c(
      "Indonesia" = "#E65639",
      "Jakarta" = "#4CCDD9",
      "Bali" = "#3DB4CC",
      "Jambi" = "#309BBF",
      "Bengkulu" = "#127DB3"
    )
  ) +
  labs(
    subtitle = "(1999 = 100)",
    x = NULL,
    y = NULL
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "#E0E0E0"),
    panel.grid.major.x = element_blank()
  )


### Bottom 10 provinces

ownershipBottom <- ownershipIndex %>%
  filter(year == 2020) %>%
  arrange(homeownership) %>%
  head(10) %>%
  mutate(province = fct_reorder(province, homeownership))

plotOwnershipBottom <- ggplot(
  ownershipBottom,
  aes(x = homeownership, y = province)
) +
  geom_col(fill = "#127DB3", color = "white", width = 0.65) +
  scale_x_continuous(
    breaks = seq(0, 100, 25),
    limits = c(0, 100),
    expand = c(0, 0),
    position = "top"
  ) +
  labs(
    subtitle = "Bottom 10 in 2020 (percent)",
    x = NULL,
    y = NULL
  ) +
  dfr_theme() +
  theme(
    axis.text.y = element_text(hjust = 0),
    axis.line.y = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    panel.grid.major.y = element_blank()
  )


### Patchwork: homeownership

plotOwnershipIndex +
  plotOwnershipBottom +
  plot_annotation(
    title = "Homeownership has fallen since 1999",
    subtitle = "Share of households with a home of their own, selected provinces",
    caption = paste0(
      "Source: Statistics Indonesia (BPS); author's analysis<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    ),
    theme = dfr_theme()
  ) +
  plot_layout(widths = c(2, 1))

ggsave(
  here(dirYear, dirProject, "result", "homeownership.png"),
  width = 8,
  height = 4.5
)


## House price index ----

price <- read_csv(here(dirYear, dirProject, "result", "house-price-index.csv"))

ggplot(price, aes(x = date, y = house_price_index_growth)) +
  geom_hline(yintercept = 0, lwd = 12/22, color = "black") +
  geom_line(aes(color = city), lwd = 1, show.legend = FALSE) +
  geom_vline(
    xintercept = as.Date("2020-04-01"),
    lwd = 0.5,
    lty = "dashed",
    color = "#757575"
  ) +
  geom_text(
    data = tibble(
      x = as.Date("2020-04-01"),
      y = 24,
      label = "COVID-19 pandemic"
    ),
    aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    color = "#757575",
    hjust = 1,
    nudge_x = -25,
    nudge_y = -2
  ) +
  add_label(
    df = tibble(x = as.Date("2015-12-01"), y = 20, label = "Manado"),
    textColor = "#127DB3"
  ) +
  add_label(
    df = tibble(x = as.Date("2015-01-01"), y = 8.5, label = "Overall"),
    textColor = "#E65639"
  ) +
  add_label(
    df = tibble(x = as.Date("2013-10-01"), y = 6, label = "Bandar\nLampung"),
    textColor = "#4CCDD9"
  ) +
  add_label(
    df = tibble(x = as.Date("2019-01-01"), y = 8.5, label = "Other cities"),
    textColor = "#757575"
  ) +
  gghighlight(
    city %in% c("Overall", "Manado", "Bandar Lampung"),
    label_key = F, # Turn off labels
    unhighlighted_params = list(color = "grey", alpha = 0.25)
  ) +
  scale_x_date(
    breaks = seq(
      as.Date("2012-01-01"),
      as.Date("2021-01-01"),
      by = "1 year"
    ),
    labels = c(
      "Q1\n2012",
      paste("'", seq(13, 21))
    )
  ) +
  scale_y_continuous(
    breaks = seq(-12, 24, 6),
    limits = c(-12, 24),
    position = "right"
  ) +
  scale_color_manual(
    values = c(
      "Overall" = "#E65639",
      "Bandar Lampung" = "#4CCDD9",
      "Manado" = "#127DB3"
    )
  ) +
  labs(
    title = "House prices have grown slower",
    subtitle = "House price index year-on-year change, by city (percent)",
    x = NULL,
    y = NULL,
    caption = "Source: Bank Indonesia (BI)<br>Chart: Dzulfiqar Fathur Rahman"
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "#E0E0E0"),
    panel.grid.major.x = element_blank()
  )

ggsave(
  here(dirYear, dirProject, "result", "house-price-index.png"),
  width = 8,
  height = 4.5
)


## Mortgage ----

mortgage <- read_csv(here(dirYear, dirProject, "result", "mortgage.csv"))

mortgageMillion <- mortgage %>%
  mutate(payment = payment / 1000000)

annotationsProvinces <- mortgageMillion %>%
  filter(
    province %in% c(
      "DKI Jakarta",
      "DI Yogyakarta",
      "Bali",
      "East Nusa Tenggara",
      "Bangka Belitung Islands",
      "Lampung"
    )
  ) %>%
  mutate(
    province = str_replace_all(
      province,
      c(
        "DKI " = "",
        "DI " = "",
        "East Nusa Tenggara" = "East Nusa\nTenggara",
        "Bangka Belitung Islands" = "Bangka Belitung\nIslands"
      )
    )
  )

ggplot(mortgageMillion, aes(x = term, y = payment)) +
  geom_point(
    pch = 21,
    color = "white",
    fill = "#1D8EBF",
    alpha = 0.75,
    size = 4.5
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "#757575",
    lwd = 0.75,
    lty = "dashed"
  ) +
  geom_label(
    data = annotationsProvinces,
    aes(label = province),
    size = dfr_convert_font_size(),
    hjust = 1,
    color = "#757575",
    nudge_x = -0.125,
    nudge_y = 0.35,
    label.size = NA,
    label.padding = unit(0.05, "lines")
  ) +
  scale_x_continuous(breaks = seq(8, 20, 2), limits = c(8, 20)) +
  scale_y_continuous(
    breaks = seq(0, 8, 2),
    limits = c(0, 8),
    expand = c(0, 0),
    position = "right"
  ) +
  labs(
    title = "Payments in Jakarta, Yogyakarta are unusually high",
    subtitle = paste0(
      "Mortgage average monthly payments and terms in 2019, ",
      "by province\\*"
    ),
    x = "Terms<br>(years)",
    y = "Payments<br>(million rupiah)",
    caption = paste0(
      "\\*Excluding West Nusa Tenggara, Maluku and North Maluku<br>",
      "Source: Statistics Indonesia (BPS)<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    )
  ) +
  dfr_theme() +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black")
  )

ggsave(
  here(dirYear, dirProject, "result", "mortgage.png"),
  width = 8,
  height = 4.5
)
