dirYear <- "2022"
dirProject <- "2022-01-04-trade-investment-china-us"

here::i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
library(ggtext)
library(dfrtheme)


# Plot ----

## Trade ----

tradeShare <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "trade-share-china-us-other.csv"
  )
)

tradeSharePrep <- tradeShare %>%
  mutate(partner = fct_relevel(partner, c("Other", "USA", "China")))

annotationTrade <- tibble(
  x = c(2001, 2010),
  y = c(1, 1),
  label = c("China<br>joined<br>WTO*", "ACFTA**"),
  trade_flow = c("Export", "Export")
)

ggplot(
  data = tradeSharePrep,
  mapping = aes(x = year, y = share_of_total)) +
  geom_area(
    mapping = aes(fill = partner),
    color = "white",
    alpha = 0.75
  ) +
  geom_vline(xintercept = c(2001, 2010), lwd = 0.5) +
  geom_richtext(
    data = annotationTrade,
    mapping = aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    text.color = "#757575",
    fill = NA,
    hjust = 1,
    vjust = 1,
    nudge_y = -0.01,
    label.size = 0,
    label.color = NA
  ) +
  scale_x_continuous(
    breaks = seq(1990, 2020, 5),
    labels = c("1990", "'95", "2000", "'05", "'10", "'15", "'20")
  ) +
  scale_y_continuous(
    labels = seq(0, 100, 25),
    limits = c(0, 1),
    expand = c(0, 0),
    position = "right"
  ) +
  scale_fill_manual(
    values = c("China" = "#E91E63", "USA" = "#0D47A1", "Other" = "#E0E0E0"),
    labels = c("China" = "China", "USA" = "United States", "Other" = "Other")
  ) +
  guides(fill = guide_legend(nrow = 1, override.aes = list(color = NA))) +
  facet_wrap(~ trade_flow) +
  labs(
    title = "China has become Indonesia's largest trading partner",
    subtitle = paste0(
      "Share of Indonesia's goods exports and imports ",
      "by trading partner (percent)"
    ),
    x = NULL,
    y = NULL,
    caption = paste0(
      "\\*World Trade Organization \\*\\*ASEAN-China Free Trade Area<br>",
      "Source: United Nations (UN); author's analyis<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    )
  ) +
  dfr_theme() +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    legend.justification = c(0, 1),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  )

ggsave(
  here(dirYear, dirProject, "result", "trade-share-china-us-other.svg"),
  width = 8,
  height = 4.75
)


## Foreign direct investment ----

fdi <- read_csv(here(dirYear, dirProject, "result", "fdi-china-us.csv"))

ggplot(
  data = fdi,
  mapping = aes(x = year, y = fdi, color = country)
) +
  geom_hline(yintercept = 0, lwd = 12/22) +
  geom_line(lwd = 1.25) +
  scale_x_continuous(
    breaks = seq(2010, 2020),
    limits = c(2010, 2020),
    labels = c("2010", paste0("'", seq(11, 20)))
  ) +
  scale_y_continuous(
    breaks = seq(-6, 4, 2),
    limits = c(-6, 4),
    position = "right"
  ) +
  scale_color_manual(
    values = c("China" = "#E91E63", "United States" = "#0D47A1")
  ) +
  labs(
    title = "More and more investment from China",
    subtitle = paste0(
      "Net foreign direct investment in Indonesia ",
      "by country (billion US dollars)"
    ),
    x = NULL,
    y = NULL,
    caption = paste0(
      "Source: Bank Indonesia<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    )
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "#E0E0E0"),
    legend.position = c(0.125, 0.875),
    panel.grid.major.x = element_blank()
  )

ggsave(
  here(dirYear, dirProject, "result", "fdi-china-us.svg"),
  width = 8,
  height = 4.5
)
