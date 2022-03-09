# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(dfrtheme)
library(ggrepel)
library(patchwork)

dirYear <- "2021"
dirProject <- "2021-12-11-affluent-retire-early"

i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Plot ----

plot_dot <- function(df,
                     fillColor = "#2477B3",
                     plotSubtitle) {

  columnsRequired <- c(
    "share_of_respondents",
    "country",
    "ci_lower",
    "ci_upper"
  )

  if (!all(columnsRequired %in% names(df))) {
    stop(
      "`df` must contain the following columns:",
      paste0(columnsRequired, collapse = ", ")
    )
  }

  ggplot2::ggplot(
    data = df,
    mapping = ggplot2::aes(x = share_of_respondents, y = country)
  ) +
    ggplot2::geom_linerange(
      mapping = ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
      color = fillColor,
      lwd = 3,
      alpha = 0.25
    ) +
    ggplot2::geom_point(
      size = 2,
      pch = 19,
      color = fillColor,
      stroke = 0.75
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 70, 10),
      limits = c(0, 70),
      expand = c(0, 0),
      position = "top"
    ) +
    ggplot2::labs(
      subtitle = plotSubtitle,
      x = NULL,
      y = NULL
    ) +
    dfrtheme::dfr_theme() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(hjust = 0),
      axis.line.y = ggplot2::element_line(color = "black"),
      axis.ticks.y = ggplot2::element_line(color = "black"),
      panel.grid.major.y = ggplot2::element_blank()
    )

}


## Retire early ----

retireBefore65 <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "retire-early.csv"
  )
)

retireBefore65prep <- retireBefore65 %>%
  mutate(country = fct_reorder(country, share_of_respondents))

plotRetireBefore65 <- plot_dot(
  retireBefore65prep,
  plotSubtitle = "Looking to retire before turning 65"
) +
  geom_label_repel(
    data = tibble(
      x = 50.5,
      y = "United Arab Emirates",
      label = "95% confidence interval"
    ),
    mapping = aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    color = "#2477B3",
    hjust = 1,
    nudge_x = -5,
    label.padding = unit(0, "lines"),
    label.size = NA
  )


## Retirement savings ----

retirementSavings <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "retirement-savings.csv"
  )
)

retirementSavingsPrep <- retirementSavings %>%
  filter(!is.na(share_of_respondents)) %>%
  mutate(country = fct_reorder(country, share_of_respondents, .desc = TRUE))

plotRetirementSavings <- plot_dot(
  retirementSavingsPrep,
  fillColor = "#E66439",
  plotSubtitle = "Who haven't started saving for retirement"
)


## Patchwork ----

plotRetireBefore65 +
  plotRetirementSavings +
  plot_annotation(
    title = "Most affluent Indonesians want to retire early",
    subtitle = "Share of respondents\\* (percent)",
    caption = paste0(
      "\\*In an online survey of 15,649 emerging affluent, affluent and ",
      "high-net-worth individuals in 12 countries between June 30<br>",
      "and July 26, 2021<br>",
      "Source: Standard Chartered; author's analysis<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    ),
    theme = dfr_theme()
  )

ggsave(
  here(dirYear, dirProject, "result", "retirement.svg"),
  width = 8.5,
  height = 5
)
