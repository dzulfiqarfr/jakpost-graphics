dirYear <- "2021"
dirProject <- "2021-12-17-yearender"

here::i_am(paste(dirYear, dirProject, "src", "analyze.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(httr)
library(jsonlite)
library(zoo)


# Data ----

## COVID-19 cases and vaccination rate ----

covidRaw <- fread(
  here(dirYear, dirProject, "data", "owid-covid-raw.csv"),
  select = c(
    "iso_code",
    "date",
    "new_cases_smoothed",
    "people_fully_vaccinated_per_hundred"
  )
)

covidIDN <- covidRaw %>%
  as_tibble() %>%
  filter(iso_code == "IDN")

covidIDN %>%
  write_csv(here(dirYear, dirProject, "result", "covid-cases-vax.csv"))


## Mobility ----

mobilityIDN <- read_csv(
  here(
    dirYear,
    dirProject,
    "data",
    "google-mobility-indonesia-cleaned.csv"
  )
)

add_moving_average <- function(...) {
  zoo::rollmean(..., k = 7, fill = NA, align = "right")
}

mobilityIDNsmoothed <- mobilityIDN %>%
  mutate(
    retail_smoothed = add_moving_average(retail),
    transit_smoothed = add_moving_average(transit),
    workplace_smoothed = add_moving_average(workplace)
  ) %>%
  select(date, ends_with("smoothed"))

mobilityIDNsmoothed %>%
  write_csv(
    here(
      dirYear,
      dirProject,
      "result",
      "mobility-retail-transit-workplace.csv"
    )
  )


## Economic growth ----

keyBPS <- Sys.getenv("keyBPS")

idGrowth <- "104"

respGrowthRaw <- GET(
  "https://webapi.bps.go.id/v1/api/list",
  query = list(
    model = "data",
    domain = "0000",
    var = idGrowth,
    key = keyBPS
  )
)

respGrowthParsed <- respGrowthRaw %>%
  content(type = "text") %>%
  fromJSON()

anchor_regex <- function(data) {

  if (!any("val" %in% names(data))) {
    stop("`data` must contain `val` column.")
  }

  dataAnchored <- data %>% dplyr::mutate(val = paste0("^", val, "$"))

  return(dataAnchored)

}

idSector <- respGrowthParsed$vervar %>% as_tibble() %>% anchor_regex()

idIndicator <- respGrowthParsed$turvar %>% as_tibble() %>% anchor_regex()

idYearGrowth <- respGrowthParsed$tahun %>% as_tibble() %>% anchor_regex()

idQuarterGrowth <- respGrowthParsed$turtahun %>% as_tibble() %>% anchor_regex()

idDateGrowth <-  c(
  "^31$" = "-01-01",
  "^32$" = "-04-01",
  "^33$" = "-07-01",
  "^34$" = "-10-01",
  "^35$" = "-12-01"
)

growthRaw <- as_tibble(respGrowthParsed$datacontent)

growthLong <- growthRaw %>%
  pivot_longer(
    cols = everything(),
    names_to = c("id_sector", "id_composite"),
    names_sep = idGrowth,
    values_to = "growth"
  )

growthClean <- growthLong %>%
  mutate(
    sector = str_replace_all(id_sector, deframe(idSector)),
    sector = str_remove_all(sector, "&lt;b&gt;"),
    sector = str_remove_all(sector, "&lt;/b&gt;"),
    sector = str_replace_all(
      sector,
      c("M,N" = "M, N", "R,S,T,U" = "R, S, T, U")
    ),
    id_indicator = str_sub(id_composite, 1, 1),
    indicator = str_replace_all(id_indicator, deframe(idIndicator)),
    id_year = str_sub(id_composite, 2, 4),
    year = str_replace_all(id_year, deframe(idYearGrowth)),
    id_quarter = str_sub(id_composite, 5, 6),
    quarter = str_replace_all(id_quarter, idDateGrowth),
    date = as.Date(paste0(year, quarter))
  ) %>%
  select(indicator, sector, date, growth)

growthSubset <- growthClean %>%
  filter(
    str_detect(indicator, "y-on-y"),
    sector == "C.  PRODUK DOMESTIK BRUTO",
    between(date, as.Date("2016-01-01"), as.Date("2021-07-01")),
    !str_detect(date, "-12-01") # Remove annual figures
  ) %>%
  select(date, growth)

growthSubset %>%
  write_csv(here(dirYear, dirProject, "result", "gdp-growth.csv"))
