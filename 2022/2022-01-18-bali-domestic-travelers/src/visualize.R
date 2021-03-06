dirYear <- "2022"
dirProject <- "2022-01-18-bali-domestic-travelers"

here::i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
library(dfrtheme)


# Plot ----

guestForeignShare <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "foreign-guest-share.csv"
  )
)

guestForeignSharePrep <- guestForeignShare %>%
  arrange(desc(foreign_guest_share)) %>%
  head(10) %>%
  mutate(province = fct_reorder(province, foreign_guest_share))

ggplot(
  data = guestForeignSharePrep,
  mapping = aes(x = foreign_guest_share, y = province)
) +
  geom_col(width = 0.7, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 3.83, lwd = 0.5) +
  geom_label(
    data = tibble(x = 3.83, y = "Yogyakarta", label = "National average"),
    mapping = aes(x = x, y = y, label = label),
    size = dfr_convert_font_size(),
    # color = "#757575",
    hjust = 0,
    nudge_x = 0.25,
    label.padding = unit(0, "lines"),
    label.size = 0
  ) +
  scale_x_continuous(
    breaks = seq(0, 60, 10),
    limits = c(0, 60),
    expand = c(0, 0),
    position = "top"
  ) +
  labs(
    title = "Bali's accommodation industry hinges on foreign tourist arrivals",
    subtitle = paste0(
      "Share of foreign guests at star and nonstar hotels in 2019, ",
      "top 10 provinces (percent)"
    ),
    x = NULL,
    y = NULL,
    caption = paste0(
      "Source: Statistics Indonesia (BPS); author's analysis<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    )
  ) +
  dfr_theme() +
  theme(
    axis.text.y = element_text(hjust = 0),
    axis.ticks.y = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major.y = element_blank()
  )

ggsave(
  here(dirYear, dirProject, "result", "foreign-guest-share.svg"),
  width = 8,
  height = 4.5
)
