# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(dfrtheme)
library(ggtext)

dirYear <- "2021"
dirProject <- "2021-07-21-food-security-rise"

i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Plot ----

foodSecurity <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "food-security.csv"
  )
)

foodSecurityPrep <- foodSecurity %>% mutate(year = as_factor(year))

paletteYear <- c("2020" = "#2477B3", "2018" = "#90A4AE")

ggplot(data = foodSecurityPrep, mapping = aes(x = food_security_index)) +
  geom_histogram(
    mapping = aes(fill = year, color = year),
    position = "identity",
    binwidth = 5,
    alpha = 0.5
  ) +
  geom_vline(
    xintercept = c(59.58, 51.29),
    color = "#757575",
    lwd = 0.5,
    lty = "dashed"
  ) +
  geom_label(
    data = tibble(
      x = c(51.29, 59.58),
      y = c(150, 115),
      label = c("Food security threshold for cities", "For regencies")
    ),
    mapping = aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    color = "#757575",
    fill = "white",
    hjust = 1,
    vjust = 0,
    nudge_x = -1,
    nudge_y = -10,
    label.padding = unit(0, "lines"),
    label.size = 0
  ) +
  geom_richtext(
    data = tibble(x = 10, y = 30, label = "More food secure &#9658;"),
    mapping = aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    color = "#757575",
    hjust = 0,
    label.padding = unit(0, "lines"),
    label.size = 0,
    label.color = NA
  ) +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  scale_y_continuous(
    breaks = seq(0, 150, 30),
    limits = c(0, 150),
    expand = c(0, 0),
    position = "right"
  ) +
  scale_color_manual(values = paletteYear) +
  scale_fill_manual(values = paletteYear) +
  labs(
    title = "More regions becoming food secure",
    subtitle = "Distribution of cities and regencies by food security index",
    x = "Food security index",
    y = "Number of cities and regencies",
    caption = paste0(
      "Source: Food Security Agency (BKP); author's analysis<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    )
  ) +
  dfr_theme() +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    legend.justification = c(0, 1),
    legend.position = c(0.1, 0.875),
    legend.key.size = unit(1, "lines"),
    legend.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank()
  )

ggsave(
  here(dirYear, dirProject, "result", "food-security.svg"),
  width = 8,
  height = 4.5
)
