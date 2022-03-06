# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(sf)
library(nusantr)
library(ggrepel)
library(dfrtheme)
library(patchwork)

dirYear <- "2021"
dirProject <- "2021-05-23-indonesia-remains-java-centric"

i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Plot ----

## Population distribution ----

popDistribution <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "population-distribution.csv"
  )
)

popDistributionDec <- popDistribution %>%
  mutate(
    island = fct_reorder(island, population_share),
    # Convert to decimal to stack the bars
    population_share = population_share / 100
  )

paletteIsland <- c(
  "Java" = "#00204DFF",
  "Sumatra" = "#31446BFF",
  "Sulawesi" = "#666970FF",
  "Bali & Nusa Tenggara" = "#958F78FF",
  "Kalimantan" = "#CBBA69FF",
  "Maluku & Papua" = "#FFEA46FF"
)

plotPop <- ggplot(
  data = popDistributionDec,
  mapping = aes(x = year, y = population_share, fill = island)
) +
  geom_col(position = "stack", width = 3.5) +
  scale_x_continuous(
    breaks = c(1971, 1980, 1990, 1995, 2000, 2010, 2020),
    labels = c("1971", "'80", "'90", "'95", "2000", "'10", "'20")
  ) +
  scale_y_continuous(
    labels = seq(0, 100, 25),
    expand = c(0, 0),
    position = "right"
  ) +
  scale_fill_manual(values = paletteIsland) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(subtitle = "Population", x = NULL, y = NULL) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    legend.position = "top"
  )


## GDP distribution ----

gdpDistribution <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "gdp-distribution.csv"
  )
)

gdpDistributionDec <- gdpDistribution %>%
  mutate(
    island = fct_reorder(island, gdp_share),
    gdp_share = gdp_share / 100 # Convert to decimal to stack the bars
  )

plotGDP <- ggplot(
  data = gdpDistributionDec,
  mapping = aes(x = year, y = gdp_share, fill = island)
) +
  geom_col(position = "stack", width = 0.6, show.legend = FALSE) +
  scale_x_continuous(
    breaks = 2010:2020,
    labels = c("2010", paste0("'", 11:20))
  ) +
  scale_y_continuous(
    labels = seq(0, 100, 25),
    expand = c(0, 0),
    position = "right"
  ) +
  scale_fill_manual(values = paletteIsland) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(subtitle = "GDP", x = NULL, y = NULL) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.line.x = element_line(color = "black")
  )


## Patchwork: population and GDP distribution ----

plotPop +
  plotGDP +
  plot_annotation(
    title = "Population and economic development remain concentrated in Java",
    subtitle = "Population and real GDP distribution by island (percent)",
    caption = paste0(
      "Source: Statistics Indonesia (BPS); author's analysis<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    ),
    theme = dfr_theme()
  )

ggsave(
  here(dirYear, dirProject, "result", "population-gdp-distribution.svg"),
  width = 10,
  height = 5.25
)


## Incoming lifetime migrants and GRDP ----

grdpMigration <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "migration-grdp.csv"
  )
)

annotationProvince <- grdpMigration %>%
  filter(
    province %in% c(
      "DKI Jakarta",
      "Kepulauan Riau",
      "Sulawesi Tengah",
      "Jawa Timur",
      "Maluku",
      "Papua Barat"
    )
  ) %>%
  mutate(
    province = str_replace_all(
      province,
      c(
        "DKI Jakarta" = "Jakarta",
        "Kepulauan Riau" = "Riau Islands",
        "Sulawesi Tengah" = "Central Sulawesi",
        "Jawa Timur" = "East Java",
        "Maluku" = "Maluku",
        "Papua Barat" = "West Papua"
      )
    )
  )

ggplot(
  data = grdpMigration,
  mapping = aes(x = grdp_per_cap, y = migrant_incoming_share)
) +
  geom_hline(yintercept = 0, lwd = 12/22) +
  geom_point(
    mapping = aes(fill = island),
    size = 4.5,
    pch = 21,
    color = "white",
    alpha = 0.75,
    stroke = 0.5
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
    color = "#757575",
    hjust = 0,
    nudge_x = 0.005,
    nudge_y = 2.5,
    label.padding = unit(0, "lines"),
    label.size = 0
  ) +
  geom_label_repel(
    data = tibble(x = 34.16901, y = 11.1, label = "Bali"),
    mapping = aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    color = "#757575",
    hjust = 0,
    nudge_x = 0.1,
    label.padding = unit(0, "lines"),
    label.size = NA
  ) +
  scale_x_log10(
    breaks = c(10, 20, 50, 100, 200),
    limits = c(10, 200)
  ) +
  scale_y_continuous(
    breaks = seq(0, 50, 10),
    limits = c(0, 50),
    position = "right"
  ) +
  scale_fill_manual(
    values = c(
      "Java" = "#1d81a2",
      "Bali & Nusa Tenggara" = "#60b4d7",
      "Sumatra" = "#00dca6",
      "Kalimantan" = "#ffca76",
      "Sulawesi" = "#ff5e4b",
      "Maluku & Papua" = "#455a64"
    )
  ) +
  labs(
    title = "Expecting higher income",
    subtitle = paste0(
      "Real GRDP per capita (2020) and incoming lifetime migrants relative ",
      "to population (2019) by province"
    ),
    x = "GRDP per capita<br>(million rupiah, log scale)",
    y = "Share of incoming migrants<br>(percent)",
    caption = paste0(
      "Source: Statistics Indonesia (BPS); author's analysis<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    )
  ) +
  dfr_theme() +
  theme(
    legend.justification = c(0, 1),
    legend.position = c(0.05, 0.95),
    legend.key.size = unit(0.75, "lines"),
    legend.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  here(dirYear, dirProject, "result", "migration-grdp.svg"),
  width = 8,
  height = 4.5
)


## Net lifetime migration ----

migration <- read_csv(
  here(
    dirYear,
    dirProject,
    "data",
    "bps-lifetime-migration-cleaned.csv"
  )
)

migrationNet <- migration %>%
  select(province, net_share) %>%
  rename("net_migrant_share" = "net_share") %>%
  # Use consistent province names to allow join with the sf object of the map
  mutate(province = str_replace_all(province, "Kep\\.", "Kepulauan"))

mapIDN <- id_map(region = "indonesia")

mapMigrationNet <- mapIDN %>%
  left_join(migrationNet, by = c("provinsi" = "province"))

ggplot(data = mapMigrationNet) +
  geom_sf(mapping = aes(fill = net_migrant_share), color = "white") +
  scale_fill_fermenter(
    breaks = c(-15, -10, -5, 0, 5, 10, 15),
    expand = c(0, 0),
    palette = "RdBu",
    direction = 1
  ) +
  labs(
    title = "Distance still matters",
    subtitle = "Net lifetime migration relative to population in 2019 (percent)",
    caption = paste0(
      "Source: Statistics Indonesia (BPS)<br>",
      "Map: Dzulfiqar Fathur Rahman"
    )
  ) +
  coord_sf() +
  dfr_theme() %+replace%
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    legend.justification = c(1, 1),
    legend.position = c(0.95, 0.95),
    legend.direction = "horizontal",
    legend.key.height = unit(0.5, "lines"),
    legend.key.width = unit(2, "lines")
  )

ggsave(
  here(dirYear, dirProject, "result", "net-lifetime-migration.svg"),
  width = 8,
  height = 4.5
)
