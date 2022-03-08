# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(readxl)
library(httr)
library(jsonlite)
library(countrycode)

dirYear <- "2021"
dirProject <- "2021-09-05-economic-emission-growth"

i_am(paste(dirYear, dirProject, "src", "analyze.R", sep = "/"))


# Data ----

## Emission and economic growth ----

emissionGDP <- read_csv(
  here(
    dirYear,
    dirProject,
    "data",
    "co2-emissions-and-gdp.csv"
  )
)

emissionGDPrenamed <- emissionGDP %>%
  rename(
    country = Entity,
    iso3c = Code,
    year = Year,
    gdp_per_cap = `GDP per capita, PPP (constant 2017 international $)`,
    emission_production_per_cap = `Per capita CO2 emissions`,
    emission_consumption_per_cap = `Per capita consumption-based CO2 emissions`
  )

# Remove regional, group aggregates
emissionGDPnoAgg <- emissionGDPrenamed %>% filter(!is.na(iso3c))


## Emission, including estimate for 2020 ----

fileEmission <- "Le_Quere_update_with_GCB2020_time_series_1990_2020_v1.0.xlsx"

emissionRaw <- read_excel(
  here(dirYear, dirProject, "data", fileEmission),
  sheet = "data",
  na = ""
)

emissionNoEmptyRow <- emissionRaw %>%
  rename(year = `MtCO2/yr`, `United States` = USA) %>%
  mutate(year = as.integer(year)) %>% # Coerce row containing text to `NA`
  filter(!is.na(year)) # Remove that row and empty rows

emissionLong <- emissionNoEmptyRow %>%
  pivot_longer(
    cols = -year,
    names_to = "country",
    values_to = "emission",
    values_transform = list(emission = as.double)
  )

emissionCoded <- emissionLong %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c")) %>%
  filter(!is.na(iso3c)) # Remove regional, group aggregates


## Population ----

populationRaw <- read_excel(
  here(
    dirYear,
    dirProject,
    "data",
    "AnnualTotPopMidYear-20210824014211.xlsx"
  ),
  sheet = "Data",
  skip = 1, # Skip table title
  na = ""
)

populationRenamed <- populationRaw %>%
  rename(iso3n = `ISO 3166-1 numeric code`, country = Location) %>%
  select(-Note)

populationCoded <- populationRenamed %>%
  mutate(iso3c = countrycode(iso3n, "iso3n", "iso3c")) %>%
  filter(!is.na(iso3c)) %>% # Remove regional, group aggregates
  select(-iso3n) %>%
  relocate(iso3c, country)

populationLong <- populationCoded %>%
  pivot_longer(
    cols = -c(iso3c, country),
    names_to = "year",
    names_transform = list(year = as.integer),
    values_to = "population"
  )


## 2020 emission per capita ----

emission2020 <- emissionCoded %>% filter(year == 2020, !is.na(emission))

population2020 <- populationLong %>% filter(year == 2020)

emissionPopulation <- emission2020 %>% left_join(population2020)

emissionPerCap2020 <- emissionPopulation %>%
  mutate(emission_production_per_cap_est = emission / population * 1000) %>%
  select(iso3c, country, year, emission_production_per_cap_est)

emissionGDP2020 <- emissionGDPnoAgg %>%
  left_join(emissionPerCap2020) %>%
  mutate(
    emission_production_per_cap = case_when(
      year == 2020 ~ emission_production_per_cap_est,
      TRUE ~ emission_production_per_cap
    )
  ) %>%
  select(-emission_production_per_cap_est)


## Emission and economic growth index ----

# Take countries to compare with Indonesia, index data to 1990 se we can see
# the decoupling (or not) trend
emissionGDPindex <- emissionGDP2020 %>%
  filter(iso3c %in% c("IDN", "SGP", "IND", "USA")) %>%
  group_by(country) %>%
  mutate(
    gdp_index = gdp_per_cap / first(gdp_per_cap) * 100,
    emission_production_index = emission_production_per_cap / first(emission_production_per_cap) * 100,
    emission_consumption_index = emission_consumption_per_cap / first(emission_consumption_per_cap) * 100
  ) %>%
  ungroup() %>%
  select(country, year, ends_with("index"))

emissionGDPindexLong <- emissionGDPindex %>%
  rename(
    GDP = gdp_index,
    `Production-based emission` = emission_production_index,
    `Consumption-based emission` = emission_consumption_index
  ) %>%
  pivot_longer(
    cols = -c(country, year),
    names_to = "category",
    values_to = "index"
  )

emissionGDPindexLong %>%
  write_csv(here(dirYear, dirProject, "result", "emission-economic-growth.csv"))


## Human development index (HDI) ----

urlHDI <- "http://ec2-54-174-131-205.compute-1.amazonaws.com/API/HDRO_API.php/country_code=IDN/indicator_id=137506"

respHDIraw <- GET(urlHDI)

respHDIparsed <- respHDIraw %>%
  content(type = "text") %>%
  fromJSON()

hdiRaw <- respHDIparsed %>%
  pluck("indicator_value", "IDN", "137506") %>%
  as_tibble()

hdiLong <- hdiRaw %>%
  pivot_longer(
    cols = everything(),
    names_to = "year",
    names_transform = list(year = as.integer),
    values_to = "hdi"
  )


## Material footprint per capita ----

materialRaw <- read_csv(
  here(
    dirYear,
    dirProject,
    "data",
    "unep-material-footprint.csv"
  ),
  skip = 3 # Skip table title and notes
)

materialRenamed <- materialRaw %>%
  rename(
    country = `COUNTRY NAME`,
    year = YEAR,
    material_footprint_per_cap = VALUE
  ) %>%
  select(-`INDICATOR NAME (unit)`)


## Planetary pressures-adjusted HDI (PHDI) ----

emissionIDN <- emissionGDP2020 %>%
  filter(country == "Indonesia") %>%
  select(year, emission_production_per_cap)

# Get the highest emission to calculate the index
emissionMax <- emissionGDP2020 %>%
  filter(
    emission_production_per_cap == max(emission_production_per_cap, na.rm = TRUE)
  ) %>%
  pull(emission_production_per_cap)

emissionIndex <- emissionIDN %>%
  mutate(
    emission_index = (emissionMax - emission_production_per_cap) / emissionMax
  ) %>%
  select(-emission_production_per_cap)

materialIDN <- materialRenamed %>%
  filter(country == "Indonesia") %>%
  select(-country)

# Get the highest material footprint to calculate the index
materialMax <- materialRenamed %>%
  filter(
    material_footprint_per_cap == max(material_footprint_per_cap, na.rm = TRUE)
  ) %>%
  pull(material_footprint_per_cap)

# Take the latest material footprint data for Indonesia, namely 2017
materialIDNlatest <- materialIDN %>%
  slice(nrow(.)) %>%
  pull(material_footprint_per_cap)

# Use the 2017 data to fill in missing values for 2018-2020 observations. This
# will underestimate the actual material footprint, given the upward trend, tho
materialIDNadded <- materialIDN %>%
  bind_rows(
    tibble(
      year = c(2018, 2019, 2020),
      material_footprint_per_cap = rep(materialIDNlatest, 3)
    )
  )

materialIndex <- materialIDNadded %>%
  mutate(
    material_index = (materialMax - material_footprint_per_cap) / materialMax
  ) %>%
  select(-material_footprint_per_cap)

# Join emission, material footprint indices to create the adjustment factor
# for planetary pressures
planetaryPressureAdjustment <- emissionIndex %>%
  left_join(materialIndex) %>%
  mutate(adjustment_factor = (emission_index + material_index) / 2)

phdi <- hdiLong %>%
  left_join(planetaryPressureAdjustment) %>%
  mutate(phdi = hdi * adjustment_factor) %>%
  select(year, hdi, phdi)

write_csv(phdi, here(dirYear, dirProject, "result", "phdi.csv"))
