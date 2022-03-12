dirYear <- "2021"
dirProject <- "2021-06-28-homeownership-slides"

here::i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(ggrepel)
library(dfrtheme)
library(patchwork)


# Plot ----

add_label <- function(df, textColor) {

  if (!any(c("x", "y", "label") %in% names(df))) {
    stop("`df` must contain `x`, `y` and `label` columns.")
  }

  ggplot2::geom_text(
    data = df,
    mapping = ggplot2::aes(x = x, y = y, label = label),
    size = dfrtheme::dfr_convert_font_size(),
    color = textColor,
    hjust = 1
  )

}

# Get the palette:
#> library(paletteer)
#> paletteer_d("werpals::benagil")


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

plotOwnershipIndex <- ggplot(
  data = ownershipIndex,
  mapping = aes(x = year, y = homeownership_index)
) +
  geom_hline(yintercept = 100, lwd = 12/22) +
  geom_line(
    mapping = aes(color = province),
    lwd = 1,
    show.legend = FALSE
  ) +
  add_label(
    df = tibble(x = 2020, y = 90, label = "National"),
    textColor = "#030710FF"
  ) +
  add_label(
    df = tibble(x = 2019, y = 115, label = "Bengkulu"),
    textColor = "#00517CFF"
  ) +
  add_label(
    df = tibble(x = 2016, y = 82.5, label = "Bali"),
    textColor = "#C05100FF"
  ) +
  add_label(
    df = tibble(x = 2018, y = 70.5, label = "Jakarta"),
    textColor = "#FBA23CFF"
  ) +
  geom_text_repel(
    data = tibble(x = 2014, y = 101.37470, label = "Jambi"),
    mapping = aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    color = "#8BA2B4FF",
    hjust = 1,
    nudge_x = -1,
    nudge_y = 7.5,
    segment.curvature = -0.25,
    segment.ncp = 3,
    segment.color = "#8BA2B4FF"
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
      "Indonesia" = "#030710FF",
      "Jakarta" = "#FBA23CFF",
      "Bali" = "#C05100FF",
      "Jambi" = "#8BA2B4FF",
      "Bengkulu" = "#00517CFF"
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

ownershipBottom10 <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "homeownership-bottom-10.csv"
  )
)

ownershipBottom10prep <- ownershipBottom10 %>%
  mutate(province = fct_reorder(province, homeownership))

plotOwnershipBottom <- ggplot(
  data = ownershipBottom10prep,
  mapping = aes(x = homeownership, y = province)
) +
  geom_col(fill = "#00517CFF", color = "white", width = 0.65) +
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
  here(dirYear, dirProject, "result", "homeownership.svg"),
  width = 8,
  height = 4.5
)


## House price index ----

price <- read_csv(here(dirYear, dirProject, "result", "house-price-index.csv"))

ggplot(
  data = price,
  mapping = aes(x = date, y = house_price_index_growth)
) +
  geom_hline(yintercept = 0, lwd = 12/22) +
  geom_line(
    mapping = aes(color = city),
    lwd = 1,
    show.legend = FALSE
  ) +
  geom_vline(
    xintercept = as.Date("2020-04-01"),
    color = "#757575",
    lwd = 0.5,
    lty = "dashed"
  ) +
  geom_text(
    data = tibble(
      x = as.Date("2020-04-01"),
      y = 24,
      label = "COVID-19 pandemic"
    ),
    mapping = aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    color = "#757575",
    hjust = 1,
    nudge_x = -25,
    nudge_y = -2
  ) +
  add_label(
    df = tibble(x = as.Date("2015-12-01"), y = 20, label = "Manado"),
    textColor = "#00517CFF"
  ) +
  add_label(
    df = tibble(x = as.Date("2015-01-01"), y = 8.5, label = "Overall"),
    textColor = "#030710FF"
  ) +
  add_label(
    df = tibble(x = as.Date("2015-02-01"), y = 15, label = "Greater Jakarta"),
    textColor = "#C05100FF"
  ) +
  add_label(
    df = tibble(x = as.Date("2013-10-01"), y = 6, label = "Bandar\nLampung"),
    textColor = "#FBA23CFF"
  ) +
  scale_x_date(
    breaks = seq(as.Date("2012-01-01"), as.Date("2021-01-01"), "1 year"),
    labels = c("Q1\n2012", paste("'", seq(13, 21)))
  ) +
  scale_y_continuous(
    breaks = seq(-12, 24, 6),
    limits = c(-12, 24),
    position = "right"
  ) +
  scale_color_manual(
    values = c(
      "Overall" = "#030710FF",
      "Bandar Lampung" = "#FBA23CFF",
      "Jabodebek-Banten" = "#C05100FF",
      "Manado" = "#00517CFF"
    )
  ) +
  labs(
    title = "House prices have grown slower",
    subtitle = paste0(
      "Residential property price index year-on-year change ",
      "by city (percent)"
    ),
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
  here(dirYear, dirProject, "result", "house-price-index.svg"),
  width = 8,
  height = 4.5
)


## Mortgage ----

mortgage <- read_csv(here(dirYear, dirProject, "result", "mortgage.csv"))

mortgageMillion <- mortgage %>% mutate(payment = payment / 1000000)

annotationProvince <- mortgageMillion %>%
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

ggplot(data = mortgageMillion, mapping = aes(x = term, y = payment)) +
  geom_point(
    size = 4.5,
    pch = 21,
    color = "white",
    fill = "#00517CFF",
    alpha = 0.75
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    lwd = 0.75,
    lty = "dashed"
  ) +
  geom_label(
    data = annotationProvince,
    mapping = aes(label = province),
    size = dfr_convert_font_size(),
    hjust = 1,
    color = "#757575",
    nudge_x = -0.125,
    nudge_y = 0.35,
    label.padding = unit(0.05, "lines"),
    label.size = NA
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
      "Mortgages: average monthly payments and terms in 2019 ",
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
  here(dirYear, dirProject, "result", "mortgage.svg"),
  width = 8,
  height = 4.5
)
