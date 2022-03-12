dirYear <- "2022"
dirProject <- "2022-02-09-deindustrialization"

here::i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
library(dfrtheme)


# Plot ----

mvaShare <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "manufacturing-value-added-share.csv"
  )
)

ggplot(data = mvaShare, mapping = aes(x = year, y = mva_share)) +
  geom_line(lwd = 1, color = "#1d81a2") +
  geom_vline(
    xintercept = c(1998, 2020),
    color = "#757575",
    lwd = 0.5,
    lty = "dashed"
  ) +
  geom_text(
    data = tibble(
      x = c(1998, 2020),
      y = c(25, 25),
      label = c("Asian financial crisis", "COVID-19")
    ),
    mapping = aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    color = "#757575",
    hjust = 1,
    nudge_x = -0.5,
    nudge_y = -1
  ) +
  scale_x_continuous(
    breaks = seq(1970, 2020, 5),
    labels = c(
      "1970", paste0("'", seq(75, 95, 5)),
      "2000", "'05", "'10", "'15", "'20"
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 25, 5),
    limits = c(0, 25),
    expand = c(0, 0),
    position = "right"
  ) +
  labs(
    title = "Deindustrialization in Indonesia",
    subtitle = "Share of manufacturing value added in GDP\\* (percent)",
    x = NULL,
    y = NULL,
    caption = paste0(
      "\\*At 2015 US dollars<br>",
      "Source: United Nations; author's analysis<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    )
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    panel.grid.major.x = element_blank()
  )

ggsave(
  here(dirYear, dirProject, "result", "manufacturing-value-added-share.svg"),
  width = 8,
  height = 4.5
)
