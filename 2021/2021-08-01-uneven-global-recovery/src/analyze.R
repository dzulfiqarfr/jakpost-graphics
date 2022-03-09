# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
conflict_prefer("last", "dplyr")
library(readxl)
library(data.table)
library(lubridate)
conflict_prefer("year", "lubridate")
conflict_prefer("month", "lubridate")
library(WDI)

dirYear <- "2021"
dirProject <- "2021-08-01-uneven-global-recovery"

i_am(paste(dirYear, dirProject, "src", "analyze.R", sep = "/"))


# Data ----

## Revision to economic growth forecast ----

weoRaw <- read_excel(
  here(dirYear, dirProject, "data", "imf-weo-2021-07-raw.xlsx"),
  sheet = "Real GDP Growth",
  na = ""
)

weoColumnNamed <- weoRaw %>%
  select_if(function(x) !all(is.na(x))) %>%
  rename(
    country = 1,
    actual_2019 = 2,
    actual_2020 = 3,
    forecast_2021 = 4,
    forecast_2022 = 5,
    revision_2021 = 6,
    revision_2022 = 7
  )

weoClean <- weoColumnNamed %>%
  filter(!is.na(country), !is.na(actual_2019)) %>%
  mutate(
    country = str_remove_all(country, "\\s\\d/"),
    country = str_replace(country, "Korea", "South Korea")
  )

weoRevision <- weoClean %>%
  select(country, starts_with("revision")) %>%
  pivot_longer(
    cols = starts_with("revision"),
    names_to = c(NA, "year"),
    names_sep = "_",
    names_transform = list(year = as.integer),
    values_to = "gdp_growth_forecast_revision",
    values_transform = list(gdp_growth_forecast_revision = as.double)
  )

weoRevisionSubset <- weoRevision %>% filter(year == 2021)

weoRevisionSubset %>%
  write_csv(here(dirYear, dirProject, "result", "weo-revision.csv"))


## Vaccination rate by income group ----

urlVax <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"

vaxRaw <- fread(
  urlVax,
  select = c(
    "iso_code",
    "location",
    "date",
    "people_vaccinated_per_hundred"
  )
)

vaxClean <- vaxRaw %>%
  filter(!str_detect(iso_code, "^OWID")) %>% # Remove aggregates
  mutate(year = year(date), month = month(date)) %>%
  group_by(iso_code) %>%
  filter(year == 2021, month < 8) %>%
  filter(!is.na(people_vaccinated_per_hundred)) %>% # Remove NAs first
  filter(date == last(date)) %>% # Then get the latest observation
  ungroup() %>%
  select(-c(year, month))

countryProfile <- as_tibble(WDI_data$country)

incomeGroup <- countryProfile %>% select(iso3c, income)

vaxIncomeGroup <- vaxClean %>%
  left_join(incomeGroup, by = c("iso_code" = "iso3c")) %>%
  filter(!is.na(income))

vaxIncomeGroup %>%
  write_csv(
    here(
      dirYear,
      dirProject,
      "result",
      "vaccination-rate-income-group.csv"
    )
  )
