# Packages ----

library(conflicted)
library(here)
conflict_prefer("here", "here")
library(tidyverse)
library(dfrtheme)
library(ggbeeswarm)
library(ggtext)
library(ggrepel)

dirYear <- "2021"
dirProject <- "2021-08-01-uneven-global-recovery"

i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Plot ----

## Revision to economic growth forecast ----

weoRevision <- read_csv(here(dirYear, dirProject, "result", "weo-revision.csv"))

weoRevisionPrep <- weoRevision %>%
  mutate(
    country = str_replace(country, "Indonesia", "<b>Indonesia</b>"),
    country = fct_reorder(country, gdp_growth_forecast_revision),
    revision = case_when(
      gdp_growth_forecast_revision > 0 ~ "Upward",
      TRUE ~ "Downward"
    ),
    revision = fct_rev(revision)
  )

ggplot(
  weoRevisionPrep,
  aes(x = gdp_growth_forecast_revision, y = country, fill = revision)
) +
  geom_segment(
    aes(xend = 0, yend = country, color = revision),
    lwd = 3,
    alpha = 0.25,
    show.legend = FALSE
  ) +
  geom_vline(
    xintercept = 0,
    lwd = 12/22,
    color = "black"
  ) +
  geom_point(
    pch = 21,
    color = "white",
    size = 3,
    show.legend = FALSE
  ) +
  scale_x_continuous(breaks = seq(-4, 4, 2), limits = c(-4, 4)) +
  scale_fill_manual(values = c("Upward" = "#324B64FF", "Downward" = "#C87D4BFF")) +
  scale_color_manual(values = c("Upward" = "#324B64FF", "Downward" = "#C87D4BFF")) +
  facet_wrap(~ revision, scales = "free_y") +
  labs(
    title = "Indonesia among countries with sluggish growth outlook",
    subtitle = paste0(
      "Revision in economic growth forecast for 2021 ",
      "between April and July outlooks ",
      "(percentage points)"
    ),
    x = NULL,
    y = NULL,
    caption = paste0(
      "Source: International Monetary Fund (IMF)<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    )
  ) +
  dfr_theme() +
  theme(
    axis.text.y = element_markdown(hjust = 0),
    panel.grid.major.y = element_blank()
  )

ggsave(
  here(dirYear, dirProject, "result", "weo-revision.png"),
  width = 8,
  height = 4.75
)


## Vaccination rate by income group ----

vaxIncomeGroup <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "vaccination-rate-income-group.csv"
  )
)

vaxIncomeGroupPrep <- vaxIncomeGroup %>%
  mutate(
    income = str_replace_all(
      income,
      c("Lower middle" = "Lower-middle", "Upper middle" = "Upper-middle")
    ),
    income = fct_reorder(income, people_vaccinated_per_hundred)
  )

vaxIncomeGroupPrep %>%
  group_by(income) %>%
  summarize(median(people_vaccinated_per_hundred)) %>%
  ungroup()

annotationIndonesia <- vaxIncomeGroupPrep %>%
  filter(location == "Indonesia")

annotationOtherCountry <- vaxIncomeGroupPrep %>%
  filter(
    location %in% c("Nepal", "Cambodia", "Mongolia", "Nauru", "Gibraltar")
  )

ggplot(
  vaxIncomeGroupPrep,
  aes(x = income, y = people_vaccinated_per_hundred)
) +
  geom_quasirandom(
    pch = 21,
    color = "white",
    fill = "#90A8C0FF",
    size = 4.5,
    alpha = 0.75
  ) +
  geom_point(
    data = annotationIndonesia,
    pch = 21,
    size = 4.5,
    color = "white",
    fill = "#304890FF",
    alpha = 0.75
  ) +
  geom_label(
    data = annotationOtherCountry,
    aes(label = location),
    size = dfr_convert_font_size(),
    hjust = 0,
    nudge_x = 0.025,
    nudge_y = 5,
    label.padding = unit(0, "lines"),
    label.size = 0,
    color = "#757575"
  ) +
  geom_text_repel(
    data = annotationIndonesia,
    aes(label = location),
    size = dfr_convert_font_size(),
    color = "#304890FF",
    hjust = 1,
    nudge_x = -0.25,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    fontface = "bold"
  ) +
  geom_richtext(
    data = tibble(x = "High income", y = 59.4, label = "Median"),
    aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    hjust = 1,
    nudge_x = -0.475,
    label.padding = unit(0, "lines"),
    label.size = 0,
    label.color = NA,
    fontface = "bold"
  ) +
  stat_summary(
    geom = "crossbar",
    fun = median,
    lwd = 0.25,
    lty = "dashed",
    color = "black"
  ) +
  scale_y_continuous(
    breaks = seq(0, 125, 25),
    limits = c(0, 125),
    expand = c(0, 0),
    position = "right"
  ) +
  labs(
    title = "Vaccine rollout is much faster in richer countries",
    subtitle = paste0(
      "Share of population vaccinated against COVID-19 ",
      "at latest date available\\*, by income group<br>",
      "(percent)"
    ),
    x = NULL,
    y = NULL,
    caption = paste0(
      "\\*As of July 31, 2021<br>",
      "Source: Our World in Data; World Bank; author's analysis<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    )
  ) +
  dfr_theme() +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    panel.grid.major.x = element_blank()
  )

ggsave(
  here(dirYear, dirProject, "result", "vaccination-rate-income-group.png"),
  width = 8,
  height = 4.5
)
