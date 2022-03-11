# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(readxl)
library(lubridate)
library(comtradr)

dirYear <- "2022"
dirProject <- "2022-01-04-trade-investment-china-us"

i_am(paste(dirYear, dirProject, "src", "analyze.R", sep = "/"))


# Data ----

## Trade ----

tradeRaw <- ct_search(
  reporters = "Indonesia",
  partners = c("China", "USA", "World"),
  trade_direction = c("exports", "imports")
)

tradeSubset <- tradeRaw %>% select(year, trade_flow, partner, trade_value_usd)

tradeWithOther <- tradeSubset %>%
  pivot_wider(names_from = partner, values_from = trade_value_usd) %>%
  group_by(year, trade_flow) %>%
  mutate(Other = World - (China + USA)) %>%
  ungroup()

tradeShare <- tradeWithOther %>%
  pivot_longer(
    cols = c(China, USA, Other),
    names_to = "partner",
    values_to = "trade_value_usd"
  ) %>%
  group_by(year, trade_flow) %>%
  mutate(share_of_total = trade_value_usd / World) %>%
  ungroup() %>%
  select(-World)

tradeShare %>%
  write_csv(
    here(
      dirYear,
      dirProject,
      "result",
      "trade-share-china-us-other.csv"
    )
  )


## Foreign direct investment ----

fdiRaw <- read_excel(
  here(dirYear, dirProject, "data", "bi-fdi-raw.xls"),
  na = c("", "-"),
  skip = 4, # Skip table title and subtitle
  sheet = "5.33"
)

fdiNoEmptyCol <- fdiRaw %>%
  select(-c(1, 2, ncol(.))) %>%
  select_if(function(x) !all(is.na(x))) %>%
  rename("country" = "COUNTRY") %>%
  relocate(country)

# Create a tibble containing column names (which include year) and quarter
# (or period) values
columnQuarter <- fdiNoEmptyCol %>%
  slice(1) %>%
  select(-c(country, NEGARA)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "column_name",
    values_to = "quarter",
    values_transform = list(quarter = as.character)
  )

# Create a year column
columnQuarterYear <- columnQuarter %>%
  mutate(
    year = case_when(str_detect(column_name, "^2") ~ column_name),
    year = case_when(
      str_detect(quarter, "Q1") & is.na(year) ~ last(year),
      TRUE ~ year
    )
  ) %>%
  fill(year)

# Create a date column with the quarter, year columns
columnNameReplacement <- columnQuarterYear %>%
  mutate(
    quarter = str_remove_all(quarter, "[:punct:]"),
    date = case_when(
      quarter == "Q1" ~ paste0(year, "-01-01"),
      quarter == "Q2" ~ paste0(year, "-04-01"),
      quarter == "Q3" ~ paste0(year, "-07-01"),
      quarter == "Q4" ~ paste0(year, "-10-01"),
      is.na(quarter) ~ paste0(year, "-12-01") # Annual figures
    )
  ) %>%
  select(date, column_name)

# Use the date column as column names
fdiColumnNamed <- fdiNoEmptyCol %>% rename(deframe(columnNameReplacement))

continent <- c(
  "North America",
  "Central and South America",
  "Europe",
  "Asia",
  "Australia and Oceania",
  "Africa",
  "Others",
  "Total"
)

fdiClean <- fdiColumnNamed %>%
  select(-NEGARA) %>%
  filter(!is.na(country)) %>%
  mutate(
    country = str_replace_all(
      country,
      c("People's Republic of China" = "China", "USA" = "United States")
    ),
    continent = case_when(country %in% continent ~ country),
    across(.cols = c(country, continent), ~ str_remove(.x, " ¹")),
    across(.cols = c(country, continent), ~ str_replace(.x, "Others", "Other"))
  ) %>%
  fill(continent) %>%
  relocate(country, continent)

fdiLong <- fdiClean %>%
  pivot_longer(
    cols = all_of(pull(columnNameReplacement, date)),
    names_to = "date",
    values_to = "fdi",
    values_transform = list(fdi = as.double)
  )

fdiSubset <- fdiLong %>%
  filter(
    country %in% c("United States", "China"),
    str_detect(date, "-12-01")
  ) %>%
  mutate(
    year = year(date),
    fdi = fdi / 1000 # Make it in billion US dollars
  ) %>%
  select(country, year, fdi)

write_csv(fdiSubset, here(dirYear, dirProject, "result", "fdi-china-us.csv"))
