# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(readxl)

dirYear <- "2022"
dirProject <- "2022-02-09-deindustrialization"

i_am(paste(dirYear, dirProject, "src", "analyze.R", sep = "/"))


# Data ----

gdpRaw <- read_excel(
  here(dirYear, dirProject, "data", "un-gdp-raw.xlsx"),
  sheet = "Download-GDPconstant-USD-countr",
  skip = 2, # Skip table title
  na = ""
)

gdpLong <- gdpRaw %>%
  pivot_longer(
    cols = `1970`:`2020`,
    names_to = "year",
    values_to = "output"
  ) %>%
  rename(
    id = CountryID,
    country = Country,
    indicator = IndicatorName
  )

indicatorGDPmanufacturing <- c(
  "Gross Domestic Product (GDP)",
  "Manufacturing (ISIC D)"
)

gdpClean <- gdpLong %>%
  filter(country == "Indonesia", indicator %in% indicatorGDPmanufacturing)

gdpSplit <- gdpClean %>% split(.$indicator)

gdp <- gdpSplit %>%
  .[["Gross Domestic Product (GDP)"]] %>%
  select(year, output) %>%
  rename("gdp" = "output")

mva <- gdpSplit %>%
  .[["Manufacturing (ISIC D)"]] %>%
  select(year, output) %>%
  rename("manufacturing_value_added" = "output")

mvaShare <- mva %>%
  left_join(gdp) %>%
  mutate(mva_share = manufacturing_value_added / gdp * 100) %>%
  select(year, mva_share)

mvaShare %>%
  write_csv(
    here(
      dirYear,
      dirProject,
      "result",
      "manufacturing-value-added-share.csv"
    )
  )
