# Packages ----

library(conflicted)
library(here)
conflict_prefer("here", "here")
library(tidyverse)
conflict_prefer("lag", "dplyr")
conflict_prefer("filter", "dplyr")
library(httr)
library(jsonlite)
library(lubridate)
library(WDI)

dirYear <- "2021"
dirProject <- "2021-06-14-covid-takes-toll-wages"

i_am(paste(dirYear, dirProject, "src", "analyze.R", sep = "/"))


# Data ----

## Average wage by sector ----

keyBPS <- Sys.getenv("keyBPS")

idVarWage <- "1521"

bps_request <- function(idVar, key) {

  respRaw <- httr::GET(
    "https://webapi.bps.go.id/v1/api/list",
    query = list(
      model = "data",
      domain = "0000",
      var = idVar,
      key = key
    )
  )

  respParsed <- respRaw %>%
    httr::content("text") %>%
    jsonlite::fromJSON()

  return(respParsed)

}

respWage <- bps_request(idVar = idVarWage, key = keyBPS)

anchor_regex <- function(data) {

  if (!any("val" %in% names(data))) {
    stop("`data` must contain `val` column.")
  }

  dataAnchored <- data %>%
    dplyr::mutate(val = paste0("^", val, "$"))

  return(dataAnchored)

}

idSector <- respWage$vervar %>% as_tibble() %>% anchor_regex()

idYearWage <- respWage$tahun %>% as_tibble() %>% anchor_regex()

idMonth <- respWage$turtahun %>% as_tibble()

wageRaw <- as_tibble(respWage$datacontent)

wageLong <- wageRaw %>%
  pivot_longer(
    cols = everything(),
    names_to = "id_composite",
    values_to = "wage"
  )

wageSep <- wageLong %>%
  separate(
    col = id_composite,
    into = c("id_sector", "id_date"),
    sep = idVarWage
  )

idSectorEn <- read_csv(here(dirYear, dirProject, "data", "id-sector.csv"))

idSectorEn <- idSectorEn %>% mutate(id = paste0("^", id, "$"))

wageClean <- wageSep %>%
  mutate(
    sector = str_replace_all(id_sector, deframe(idSector)),
    sector = str_replace_all(sector, deframe(idSectorEn)),
    # Remove the `0` prefix, which means empty `turvar`, to allow a more
    # efficient string subsetting to get the `date`
    id_date = str_remove_all(id_date, "^0"),
    year = str_sub(id_date, 1, 3),
    year = str_replace_all(year, deframe(idYearWage)),
    month = str_sub(id_date, 4, 6),
    month = str_replace_all(month, c("^189$" = "02-01", "^190$" = "08-01")),
    date = paste0(year, "-", month),
    date = ymd(date)
  ) %>%
  select(sector, date, wage)


## Consumer price index ----

indicatorCPI <- WDIsearch("consumer price index") %>%
  as_tibble() %>%
  filter(name == "Consumer price index (2010 = 100)")

cpiRaw <- WDI(indicatorCPI$indicator, country = "ID")

cpiClean <- cpiRaw %>%
  as_tibble() %>%
  rename("cpi" = "FP.CPI.TOTL") %>%
  select(year, cpi) %>%
  arrange(year) %>%
  filter(year >= 2015)


## Real wage growth ----

wageCPI <- wageClean %>%
  mutate(year = year(date)) %>%
  rename("wage_nominal" = "wage") %>%
  left_join(cpiClean)

wageReal <- wageCPI %>%
  mutate(cpi_decimal = cpi / 100, wage_real = wage_nominal / cpi_decimal) %>%
  select(sector, date, year, cpi, cpi_decimal, wage_nominal, wage_real)

wageGrowth <- wageReal %>%
  mutate(month = month(date)) %>%
  group_by(month, sector) %>%
  mutate(
    wage_nominal_diff = wage_nominal - lag(wage_nominal),
    wage_nominal_growth = wage_nominal_diff / lag(wage_nominal) * 100,
    wage_real_diff = wage_real - lag(wage_real),
    wage_real_growth = wage_real_diff / lag(wage_real) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(cpi), !is.na(wage_nominal_growth)) %>%
  select(-c(contains("diff"), month, year))

wageGrowth %>%
  filter(sector == "Overall") %>%
  write_csv(here(dirYear, dirProject, "result", "wage-growth.csv"))


## Minimum wage by province ----

wageMinRaw <- read_csv(
  here(dirYear, dirProject, "data", "wage-2021-02-01-cleaned.csv")
)

provinceNameEn <- read_csv(
  here(dirYear, dirProject, "data", "province-name.csv")
)

provinceNameEn <- provinceNameEn %>%
  mutate(province_id = paste0("^", province_id, "$"))

wageMinClean <- wageMinRaw %>%
  select(-c(starts_with("growth"), `2020-08-01`)) %>%
  pivot_longer(
    cols = c(`2020-02-01`, `2021-02-01`),
    names_to = "date",
    values_to = "wage_nominal"
  ) %>%
  mutate(province = str_replace_all(province, deframe(provinceNameEn)))

wageMinClean %>%
  write_csv(here(dirYear, dirProject, "result", "wage-minimum.csv"))


## Unemployment rate ----

idVarWorkforce <- "529"

respWorkforce <- bps_request(idVar = idVarWorkforce, key = keyBPS)

idCategory <- respWorkforce$vervar %>% as_tibble() %>% anchor_regex()

idYearWorkforce <- respWorkforce$tahun %>% as_tibble() %>% anchor_regex()

workforceRaw <- as_tibble(respWorkforce$datacontent)

workforceLong <- workforceRaw %>%
  pivot_longer(
    cols = everything(),
    names_to = "id_composite",
    values_to = "value"
  )

workforceSep <- workforceLong %>%
  separate(
    col = id_composite,
    into = c("id_category", "id_date"),
    sep = idVarWorkforce
  )

workforceClean <- workforceSep %>%
  mutate(
    category = str_replace_all(id_category, deframe(idCategory)),
    # Remove `0` prefix in `id_date` for the same reason we did with `wageSep`
    id_date = str_remove_all(id_date, "^0"),
    year = case_when(
      str_detect(id_date, "^1") ~ str_sub(id_date, 1, 3),
      TRUE ~ str_sub(id_date, 1, 2)
    ),
    year = str_replace_all(year, deframe(idYearWorkforce)),
    month = case_when(
      str_detect(id_date, "^1") ~ str_sub(id_date, 4, 6),
      TRUE ~ str_sub(id_date, 3, 5)
    ),
    month = str_replace_all(
      month,
      c("189" = "02-01", "190" = "08-01", "191" = "12-01")
    ),
    date = paste0(year, "-", month),
    date = ymd(date)
  ) %>%
  select(category, date, value)

unempClean <- workforceClean %>%
  mutate(year = year(date)) %>%
  filter(
    category == "d. Tingkat Pengangguran Terbuka (%)",
    # Match the period of observations to `wageGrowth`
    between(year, 2016, 2020)
  ) %>%
  select(date, value) %>%
  rename("unemployment_rate" = "value")

unempClean %>%
  write_csv(here(dirYear, dirProject, "result", "unemployment-rate.csv"))


## Growth in value added to GDP by sector ----

idVarGrowth <- "104"

respGrowth <- bps_request(idVar = idVarGrowth, key = keyBPS)

idSector <- respGrowth$vervar %>% as_tibble() %>% anchor_regex()

idIndicator <- respGrowth$turvar %>% as_tibble() %>% anchor_regex()

idYearGrowth <- respGrowth$tahun %>% as_tibble() %>% anchor_regex()

idQuarterGrowth <- respGrowth$turtahun %>% as_tibble() %>% anchor_regex()

idDateGrowth <-  c(
  "^31$" = "01-01",
  "^32$" = "04-01",
  "^33$" = "07-01",
  "^34$" = "10-01",
  "^35$" = "12-01"
)

growthRaw <- as_tibble(respGrowth$datacontent)

growthLong <- growthRaw %>%
  pivot_longer(
    cols = everything(),
    names_to = "id_composite",
    values_to = "growth"
  )

growthSep <- growthLong %>%
  separate(
    col = id_composite,
    into = c("id_sector", "id_date"),
    sep = idVarGrowth
  )

growthClean <- growthSep %>%
  mutate(
    sector = str_replace_all(id_sector, deframe(idSector)),
    sector = str_remove_all(sector, "&lt;b&gt;"),
    sector = str_remove_all(sector, "&lt;/b&gt;"),
    sector = str_replace_all(
      sector,
      c("M,N" = "M, N", "R,S,T,U" = "R, S, T, U")
    ),
    id_indicator = str_sub(id_date, 1, 1),
    indicator = str_replace_all(id_indicator, deframe(idIndicator)),
    id_year = str_sub(id_date, 2, 4),
    year = str_replace_all(id_year, deframe(idYearGrowth)),
    id_quarter = str_sub(id_date, 5, 6),
    quarter = str_replace_all(id_quarter, idDateGrowth),
    date = paste0(year, "-", quarter)
  ) %>%
  select(indicator, sector, date, growth)

lettersCap <- toupper(letters)

sectorsDrop <- c(
  "A. NILAI TAMBAH BRUTO ATAS HARGA DASAR",
  "B. PAJAK DIKURANG SUBSIDI ATAS PRODUK",
  "C.  PRODUK DOMESTIK BRUTO",
  "Industri Pengolahan Non Migas"
)

growthSector <- growthClean %>%
  mutate(sector_prefix = str_sub(sector, 1, 1)) %>%
  filter(
    str_detect(indicator, "y-on-y"),
    sector_prefix %in% lettersCap,
    !(sector %in% sectorsDrop),
    date == "2020-12-01"
  ) %>%
  mutate(
    sector_prefix = case_when(
      str_detect(sector, "^M") ~ str_sub(sector, 1, 4),
      str_detect(sector, "^R") ~ str_sub(sector, 1, 10),
      TRUE ~ str_sub(sector, 1, 1)
    ),
    sector = str_replace_all(sector_prefix, deframe(idSectorEn)),
    year = year(date)
  ) %>%
  select(sector, year, growth) %>%
  rename("value_added_growth" = "growth")


## Real wage and value added by sector growth

wageValueAddedGrowth <- wageGrowth %>%
  filter(date == "2020-08-01", sector != "Overall") %>%
  left_join(growthSector, by = "sector") %>%
  select(sector, year, wage_real_growth, value_added_growth)

wageValueAddedGrowth %>%
  write_csv(here(dirYear, dirProject, "result", "wage-value-added-growth.csv"))
