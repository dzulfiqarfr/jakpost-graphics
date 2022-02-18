# Packages ----

library(conflicted)
library(here)
conflict_prefer("here", "here")
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(dfrtheme)
library(ggtext)

dirYear <- "2021"
dirProject <- "2021-07-21-food-security-rise"

i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Plot ----

foodSecurity <- read_csv(here(dirYear, dirProject, "result", "food-security.csv"))

foodSecuritySubset <- foodSecurity %>% filter(year != 2019)

ggplot(
  foodSecuritySubset,
  aes(x = food_security_index)
) +
  geom_histogram(
    aes(fill = as_factor(year), color = as_factor(year)),
    position = "identity",
    binwidth = 5,
    alpha = 0.5
  ) +
  geom_vline(
    xintercept = c(59.58, 51.29),
    lwd = 0.5,
    lty = "dashed",
    color = "#757575"
  ) +
  geom_label(
    data = tibble(
      x = c(51.29, 59.58),
      y = c(150, 120),
      label = c("Food security threshold for cities", "For regencies")
    ),
    aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    vjust = 0,
    hjust = 1,
    nudge_x = -1,
    nudge_y = -10,
    color = "#757575",
    label.padding = unit(0, "lines"),
    label.size = 0,
    fill = "white"
  ) +
  geom_richtext(
    data = tibble(x = 10, y = 30, label = "More food secure &#9658;"),
    aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    hjust = 0,
    color = "#757575",
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
  scale_color_manual(values = c("2020" = "#2477B3", "2018" = "#90A4AE")) +
  scale_fill_manual(values = c("2020" = "#2477B3", "2018" = "#90A4AE")) +
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
    panel.grid.major.x = element_blank(),
    legend.justification = c(0, 1),
    legend.position = c(0.1, 0.875),
    legend.key.size = unit(0.75, "lines"),
    legend.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  here(dirYear, dirProject, "result", "food-security.png"),
  width = 8,
  height = 4.5
)
