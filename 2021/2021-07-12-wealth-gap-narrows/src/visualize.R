# Packages ----

library(conflicted)
library(here)
conflict_prefer("here", "here")
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(dfrtheme)
library(ggrepel)

dirYear <- "2021"
dirProject <- "2021-07-12-wealth-gap-narrows"

i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Plot ----

## Wealth growth between 2000 and 2020 ----

wealthWeightedGrowth <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "wealth-contribution-growth.csv"
  )
)

wealthWeightedGrowthPrep <- wealthWeightedGrowth %>%
  mutate(category = fct_reorder(category, weighted_growth))

ggplot(
  wealthWeightedGrowthPrep,
  aes(x = year, y = weighted_growth)
) +
  geom_col(
    aes(fill = category),
    color = "white",
    lwd = 0.25,
    width = 0.65
  ) +
  geom_hline(
    yintercept = 0,
    lwd = 12/22,
    color = "black"
  ) +
  geom_vline(
    xintercept = c(2008, 2020),
    lwd = 0.5,
    lty = "dashed",
    color = "#757575"
  ) +
  geom_text(
    data = tibble(
      x = c(2008, 2020),
      y = c(45, 45),
      label = c("Global financial crisis", "COVID-19 pandemic")
    ),
    aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    color = "#757575",
    vjust = 1,
    hjust = 1,
    nudge_y = -2.5,
    nudge_x = -0.25
  ) +
  scale_x_continuous(
    breaks = seq(2002, 2020, 2),
    labels = c("2002", paste0("'0", seq(4, 8, 2)), paste0("'", seq(10, 20, 2)))
  ) +
  scale_y_continuous(
    breaks = seq(-30, 45, 15),
    limits = c(-30, 45),
    position = "right"
  ) +
  scale_fill_manual(
    values = c(
      "Nonfinancial asset" = "#19324BFF",
      "Financial asset" = "#4B647DFF",
      "Debt" = "#C89664FF"
    )
  ) +
  labs(
    title = "Indonesia sees slower wealth growth, relies on nonfinancial assets",
    subtitle = paste0(
      "Contribution to year-on-year growth in net wealth per adult ",
      "by asset (percentage points)"
    ),
    x = NULL,
    y = NULL,
    caption = paste0(
      "Source: Credit Suisse; author's analysis<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    )
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "#E0E0E0"),
    panel.grid.major.x = element_blank(),
    legend.justification = c(0, 0),
    legend.position = "top",
    legend.key.size = unit(1, "lines")
  )

ggsave(
  here(dirYear, dirProject, "result", "wealth-contribution-growth.png"),
  width = 7.5,
  height = 4.25
)


## Wealth distribution ----

wealthDisCumulative <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "wealth-distribution.csv"
  )
)

wealthDisCumulativePrep <- wealthDisCumulative %>%
  group_by(year) %>%
  mutate(decile = seq(10, 100, 10), year = as_factor(year)) %>%
  ungroup()

areaInequality <- wealthDisCumulativePrep %>%
  filter(year == 2020) %>%
  mutate(perfect_distribution = seq(10, 100, 10)) %>%
  select(decile, wealth_distribution_cumulative, perfect_distribution)

areaShrinkInequality <- wealthDisCumulative %>%
  split(.$year) %>%
  bind_cols() %>%
  select(contains("cumulative")) %>%
  mutate(decile = seq(10, 100, 10)) %>%
  rename(
    "wealth_dis_cum_2019" = "wealth_distribution_cumulative...5",
    "wealth_dis_cum_2020" = "wealth_distribution_cumulative...10"
  )


ggplot() +
  geom_label(
    data = tibble(x = 80, y = 50, label = "Shaded areas\nrepresent inequality"),
    aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    color = "#757575",
    hjust = 1,
    nudge_x = 2.5,
    label.padding = unit(0, "lines"),
    label.size = 0
  ) +
  geom_ribbon(
    data = areaInequality,
    aes(
      x = decile,
      ymin = wealth_distribution_cumulative,
      ymax = perfect_distribution
    ),
    fill = "#757575",
    alpha = 0.15
  ) +
  geom_ribbon(
    data = areaShrinkInequality,
    aes(
      x = decile,
      ymin = wealth_dis_cum_2019,
      ymax = wealth_dis_cum_2020
    ),
    fill = "#36B3D9",
    alpha = 0.15
  ) +
  geom_hline(
    yintercept = 0,
    lwd = 12/22,
    color = "black"
  ) +
  geom_line(
    data = wealthDisCumulativePrep,
    aes(x = decile, y = wealth_distribution_cumulative, color = year),
    lwd = 1
  ) +
  geom_abline(
    slope = 1,
    color = "black",
    lwd = 0.5,
    lty = "dashed"
  ) +
  scale_x_continuous(
    breaks = seq(10, 100, 10),
    limits = c(10, 100)
  ) +
  scale_y_continuous(
    breaks = seq(-25, 100, 25),
    limits = c(-25, 100),
    position = "right"
  ) +
  scale_color_manual(values = c("2019" = "#36B3D9", "2020" = "#127DB3")) +
  geom_text(
    data = tibble(x = 55, y = 60, label = "Equality"),
    aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    color = "black",
    nudge_y = 2.5,
    angle = 15
  ) +
  geom_label_repel(
    data = tibble(x = 82.5, y = 22.5, label = "Inequality shrinks"),
    aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    hjust = 1,
    color = "#36B3D9",
    nudge_y = -22.5,
    nudge_x = -2.5,
    segment.curvature = 0.25,
    segment.ncp = 3,
    segment.angle = 20,
    label.size = NA,
    label.padding = unit(0, "lines")
  ) +
  labs(
    title = "Wealth gap narrows during pandemic",
    subtitle = "Wealth distribution in Indonesia (percent)",
    x = "Cumulative share of adult population from poorest to wealthiest",
    y = "Cumulative share of wealth",
    caption = paste0(
      "Source: Credit Suisse; author's analysis<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    )
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "#E0E0E0"),
    legend.justification = c(0, 1),
    legend.position = c(0.075, 0.925),
    legend.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  here(dirYear, dirProject, "result", "wealth-distribution.png"),
  width = 7.5,
  height = 4.25
)
