# Packages ----

library(conflicted)
library(here)
conflict_prefer("here", "here")
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(dfrtheme)
library(patchwork)

dirYear <- "2021"
dirProject <- "2021-05-23-indonesia-remains-java-centric"

i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Plot ----

## Population distribution ----

popDistribution <- read_csv(
  here(dirYear, dirProject, "result", "population-distribution.csv")
)

popDistributionDec <- popDistribution %>%
  mutate(
    island = fct_reorder(island, population_share),
    population_share = population_share / 100
  )

palIsland <- c(
  "Java" = "#00204DFF",
  "Sumatra" = "#31446BFF",
  "Sulawesi" = "#666970FF",
  "Bali & Nusa Tenggara" = "#958F78FF",
  "Kalimantan" = "#CBBA69FF",
  "Maluku & Papua" = "#FFEA46FF"
)

plotPop <- ggplot(
  popDistributionDec,
  aes(x = year, y = population_share, fill = island)
) +
  geom_col(position = "stack", width = 3.5) +
  scale_x_continuous(labels = c("1970", "'80", "'90", "2000", "'10", "'20")) +
  scale_y_continuous(
    labels = seq(0, 100, 25),
    expand = c(0, 0),
    position = "right"
  ) +
  scale_fill_manual(values = palIsland) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    subtitle = "Population",
    x = NULL,
    y = NULL
  ) +
  dfr_theme() +
  theme(
    legend.position = "top",
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black")
  )


## GRDP distribution ----

GDPdistribution <- read_csv(
  here(dirYear, dirProject, "result", "gdp-distribution.csv")
)

GDPdistributionDec <- GDPdistribution %>%
  mutate(
    island = fct_reorder(island, gdp_share),
    gdp_share = gdp_share / 100
  )

plotGDP <- ggplot(
  GDPdistributionDec,
  aes(x = year, y = gdp_share, fill = island)
) +
  geom_col(position = "stack", width = 0.6, show.legend = FALSE) +
  scale_x_continuous(
    breaks = seq(2010, 2020, 2),
    labels = c("2010", "'12", "'14", "'16", "'18", "'20")
  ) +
  scale_y_continuous(
    labels = seq(0, 100, 25),
    expand = c(0, 0),
    position = "right"
  ) +
  scale_fill_manual(values = palIsland) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    subtitle = "GDP",
    x = NULL,
    y = NULL
  ) +
  dfr_theme() +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black")
  )


## Patchwork ----

plotPop +
  plotGDP +
  plot_annotation(
    title = "Population, economic development remain Java-centric",
    subtitle = "Population and GDP distribution, by island (percent)",
    caption = paste0(
      "Source: Statistics Indonesia (BPS); author's analysis<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    ),
    theme = dfr_theme()
  )

ggsave(
  here(dirYear, dirProject, "result", "population-gdp-distribution.png"),
  width = 10,
  height = 5
)
