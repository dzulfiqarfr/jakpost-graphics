dirYear <- "2022"
dirProject <- "2022-03-05-low-paying-service-sectors"

here::i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(dfrtheme)


# Plot ----

laborServiceEduDist <- read_csv(
  here(
    dirYear,
    dirProject,
    "result",
    "labor-service-sector-distribution-by-education.csv")
)

labelSector <- c(
  "Wholesale and retail trade; repair of motor vehicles and motorcycles" = "Wholesale and retail trade; vehicle repair",
  "Public administration and defence; compulsory social security" = "Public administration and defence; social security"
)

levelEdu <- c(
  "University",
  "Vocational school",
  "Senior high school",
  "Junior high school",
  "Elementary school or lower",
  "None"
)

levelSector <- laborServiceEduDist %>%
  filter(education == "University") %>%
  mutate(sector = str_replace_all(sector, labelSector)) %>%
  arrange(desc(worker_share_edu)) %>%
  pull(sector)

laborServiceEduDistPrep <- laborServiceEduDist %>%
  mutate(
    sector = str_replace_all(sector, labelSector),
    sector = fct_relevel(sector, levelSector),
    education = fct_relevel(education, levelEdu),
    worker_share_edu = worker_share_edu / 100
  )

# Get the palette:
#> library(paletteer)
#> paletteer_d("Redmonder::sPBIYlGn")

paletteEducation <- c(
  "University" = "#005C55FF",
  "Vocational school" = "#3F7F68FF",
  "Senior high school" = "#7DA37BFF",
  "Junior high school" = "#BCC68DFF",
  "Elementary school or lower" = "#FAE9A0FF",
  "None" = "grey"
)

ggplot(
  data = laborServiceEduDistPrep,
  mapping = aes(x = worker_share_edu, y = sector)
) +
  geom_col(
    mapping = aes(fill = education),
    color = "white",
    # alpha = 0.75,
    width = 0.65,
    lwd = 0.5,
    position = "stack"
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.1),
    labels = seq(0, 100, 10),
    limits = c(0, 1),
    expand = c(0, 0),
    position = "top"
  ) +
  scale_fill_manual(values = paletteEducation) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +
  labs(
    title = paste0(
      "Most workers in traditional service sectors ",
      "do not have a college degree"
    ),
    subtitle = paste0(
      "Distribution of workers in services by sector and ",
      "educational attainment in 2021 (percent)"
    ),
    x = NULL,
    y = NULL,
    caption = paste0(
      "Source: Statistisc Indonesia (BPS); author's analysis<br>",
      "Chart: Dzulfiqar Fathur Rahman"
    )
  ) +
  dfr_theme() %+replace%
  theme(
    axis.text.y = element_text(hjust = 0),
    axis.ticks.y = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    legend.justification = c(0, 1),
    legend.position = c(-0.425, 1.1275),
    legend.key.size = unit(0.75, "lines"),
    plot.subtitle = element_text(hjust = 0, margin = margin(b = 25)),
    plot.margin = margin(r = 9)
  )

ggsave(
  here(
    dirYear,
    dirProject,
    "result",
    "labor-service-sector-distribution-by-education.svg"
  ),
  width = 11,
  height = 5.5
)
