dirYear <- "2021"
dirProject <- "2021-09-05-economic-emission-growth"

here::i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
library(dfrtheme)
library(ggrepel)


# Plot ----

## Economic, emission growth ----

emissionGDP <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "emission-economic-growth.csv"
  )
)

emissionGDPprep <- emissionGDP %>%
  mutate(
    category = fct_relevel(
      category,
      c("GDP", "Production-based emission", "Consumption-based emission")
    )
  )

annotationCrises <- tribble(
  ~xmin, ~xmax,
  1997, 1998,
  2007, 2008,
  2020, 2021
)

ggplot(data = emissionGDPprep) +
  geom_hline(yintercept = 100, lwd = 12/22) +
  geom_line(mapping = aes(x = year, y = index, color = category), lwd = 1) +
  geom_rect(
    data = annotationCrises,
    mapping = aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 400),
    fill = "#757575",
    alpha = 0.25
  ) +
  geom_label_repel(
    data = tibble(x = 2008, y = 382.5, label = "Crises", country = "India"),
    mapping = aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    color = "#757575",
    hjust = 1,
    vjust = 0.5,
    nudge_x = -3.5,
    label.padding = unit(0, "lines"),
    label.size = NA
  ) +
  scale_x_continuous(
    breaks = seq(1990, 2020, 10),
    labels = c("1990", "2000", "'10", "'20")
  ) +
  scale_y_continuous(
    breaks = seq(0, 400, 100),
    limits = c(0, 400),
    expand = c(0, 0),
    position = "right"
  ) +
  scale_color_manual(
    values = c(
      "GDP" = "black",
      "Production-based emission" = "#E66439",
      "Consumption-based emission" = "#F2AA61"
    )
  ) +
  labs(
    title = "Indonesia lags behind in decoupling economic growth and emissions",
    subtitle = paste0(
      "Real GDP per capita and production and consumption-based CO2 emissions ",
      "per capita\\* (1990 = 100)"
    ),
    x = NULL,
    y = NULL,
    caption = paste0(
      "\\*Production-based CO2 emissions in 2020 are estimates<br>",
      paste0(
        "Source: Our World in Data; Le Quere et al. 2021; ",
        "Global Carbon Project; United Nations; author's analysis<br>"
      ),
      "Chart: Dzulfiqar Fathur Rahman"
    )
  ) +
  facet_wrap(~ country, scale = "free_x", nrow = 1) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "#E0E0E0"),
    legend.justification = c(0, 1),
    legend.position = "top",
    legend.key.height = unit(0.25, "lines"),
    legend.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(1.5, "lines")
  )

ggsave(
  here(dirYear, dirProject, "result", "emission-economic-growth.svg"),
  width = 9.5,
  height = 4.5
)


## Planetary pressure-adjusted human development index (PHDI) ----

phdi <- read_csv(here(dirYear, dirProject, "result", "phdi.csv"))

phdiLong <- phdi %>%
  rename("HDI" = "hdi", "PHDI" = "phdi") %>%
  pivot_longer(
    cols = c(HDI, PHDI),
    names_to = "category",
    values_to = "index"
  )

ggplot(data = phdiLong, mapping = aes(x = year, y = index)) +
  geom_line(mapping = aes(color = category), lwd = 1) +
  geom_hline(
    yintercept = c(0.55, 0.7),
    color = "#757575",
    lwd = 12/22,
    lty = "dashed"
  ) +
  geom_label(
    data = tibble(
      x = c(1990, 2010),
      y = c(0.7, 0.55),
      label = c("High human development", "Medium human development")
    ),
    mapping = aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    color = "#757575",
    hjust = 0,
    label.padding = unit(0, "lines"),
    label.size = 0
  ) +
  scale_x_continuous(
    breaks = seq(1991, 2019, 4),
    labels = c("1991", "'95", "'99", "2003", "'07", paste0("'", seq(11, 19, 4)))
  ) +
  scale_y_continuous(
    breaks = seq(0.4, 0.8, 0.1),
    limits = c(0.4, 0.8),
    position = "right"
  ) +
  scale_color_manual(values = c("HDI" = "black", "PHDI" = "#E66439")) +
  labs(
    title = "CO2 emissions, resource use lower Indonesia's human development",
    subtitle = paste0(
      "Human development index (HDI) and ",
      "planetary pressures-adjusted HDI (PHDI)"
    ),
    x = NULL,
    y = NULL,
    caption = paste0(
      "Source: United Nations; Global Carbon Project; Our World in Data; ",
      "author's analysis<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    )
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "#757575"),
    legend.justification = c(0, 1),
    legend.position = c(0.05, 0.965),
    panel.grid.major.x = element_blank()
  )

ggsave(
  here(dirYear, dirProject, "result", "phdi.svg"),
  width = 8,
  height = 4.25
)
