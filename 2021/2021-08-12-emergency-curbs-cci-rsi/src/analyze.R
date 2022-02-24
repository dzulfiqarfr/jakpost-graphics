# Packages ----

library(conflicted)
library(here)
conflict_prefer("here", "here")
library(tidyverse)
conflict_prefer("filter", "dplyr")
conflict_prefer("between", "dplyr")
conflict_prefer("last", "dplyr")
conflict_prefer("lag", "dplyr")
library(readxl)
library(data.table)
library(httr)
library(jsonlite)
library(lubridate)
conflict_prefer("year", "lubridate")
library(zoo)

dirYear <- "2021"
dirProject <- "2021-08-12-emergency-curbs-cci-rsi"

i_am(paste(dirYear, dirProject, "src", "analyze.R", sep = "/"))


# Data ----

## Consumer confidence index ----

cciRaw <- read_excel(
  here(dirYear, dirProject, "data", "bi-cci.xlsx"),
  sheet = "Tabel 1",
  na = c("", "-")
)

cciCompleteCategory <- cciRaw %>%
  # Get the categories into a single column, namely `...4`
  mutate(...4 = case_when(is.na(...4) ~ ...2, TRUE ~ ...4)) %>%
  rename("category_id" = "...4", "category_en" = "...123") %>%
  relocate(category_id, category_en)

cciNoEmptyRowCol <- cciCompleteCategory %>%
  filter(!is.na(...5)) %>% # Remove rows with no values
  select_if(function(x) !all(is.na(x))) %>%
  select(-c(`Tabel 1.`, `Table 1.`, ...2)) # Remove columns with no values

# Create a tibble of long data containing year values, which are stored in the
# second row, and the column names
cciColumnYear <- cciNoEmptyRowCol %>%
  slice(1) %>%
  pivot_longer(
    cols = everything(),
    names_to = "column_name",
    values_to = "year"
  ) %>%
  filter(!is.na(year))

# Repeat the same operation to create another tibble for month values
cciColumnMonth <- cciNoEmptyRowCol %>%
  slice(2) %>%
  pivot_longer(
    cols = everything(),
    names_to = "column_name",
    values_to = "month"
  ) %>%
  filter(!is.na(month))

# Turn the month values from abbreviated month names to `-%m-01`
cciColumnMonthFormatted <- cciColumnMonth %>%
  mutate(
    month = str_replace_all(
      month,
      c(
        "Jan" = "-01-01",
        "Feb" = "-02-01",
        "Mar" = "-03-01",
        "Apr" = "-04-01",
        "Mei" = "-05-01",
        "Jun" = "-06-01",
        "Jul" = "-07-01",
        "Ags" = "-08-01",
        "Sep" = "-09-01",
        "Okt" = "-10-01",
        "Nov" = "-11-01",
        "De[c|s]" = "-12-01"
      )
    )
  )

# Create a `date` column by combining the `year` and `month` columns from the
# two tibbles created earlier
cciColumnDate <- cciColumnMonthFormatted %>%
  left_join(cciColumnYear) %>%
  fill(year) %>%
  # Keep `date` in character type so we can use it to rename columns later
  mutate(date = paste0(year, month)) %>%
  select(date, column_name)

# Rename columns using `cciColumnDate`
cciColumnNamed <- cciNoEmptyRowCol %>%
  slice(-c(1, 2)) %>%  # Remove rows containing year and month values
  rename(deframe(cciColumnDate))

cciLong <- cciColumnNamed %>%
  pivot_longer(
    cols = all_of(pull(cciColumnDate, date)),
    names_to = "date",
    names_transform = list(date = ymd),
    values_to = "index",
    values_transform = list(index = as.double)
  )

cciClean <- cciLong %>%
  mutate(year = year(date)) %>%
  filter(str_detect(category_en, "CCI"), year >= 2017) %>%
  select(date, index) %>%
  rename("consumer_confidence_index" = "index")

cciClean %>%
  write_csv(here(dirYear, dirProject, "result", "consumer-confidence-index.csv"))


## Retail sales index ----

rsiRaw <- read_excel(
  here(dirYear, dirProject, "data", "bi-rsi.xlsx"),
  sheet = "Tabel 2",
  na = c("", "-")
)

rsiNoEmptyRow <- rsiRaw %>%
  filter(!is.na(...2)) %>%
  rename(
    "category_id" = "Tabel 2. Pertumbuhan Tahunan Penjualan Riil (%, yoy)",
    "category_en" = "Table 2. Annual Growth of Retail Sales Index (%, yoy)"
  ) %>%
  relocate(category_id, category_en)

# Repeat the same steps taken when cleaning the consumer confidence data.
# Create a tibble of long data containing year values, which are stored in the
# second row, and the column names
rsiColumnYear <- rsiNoEmptyRow %>%
  slice(1) %>%
  pivot_longer(
    cols = everything(),
    names_to = "column_name",
    values_to = "year",
    values_transform = list(year = as.integer)
  ) %>%
  filter(!is.na(year))

# Repeat the same operation to create another tibble for month values
rsiColumnMonth <- rsiNoEmptyRow %>%
  slice(2) %>%
  pivot_longer(
    cols = everything(),
    names_to = "column_name",
    values_to = "month"
  ) %>%
  filter(!is.na(month))

# Turn the month values from abbreviated month names to `-%m-01`. The full month
# names in retail sales index data are in Indonesian and have asterisk prefixes
rsiColumnMonthFormatted <- rsiColumnMonth %>%
  mutate(
    month = str_remove_all(month, "[:punct:]"),
    month = str_sub(month, 1, 3),
    month = str_replace_all(
      month,
      c(
        c(
          "Jan" = "-01-01",
          "Feb" = "-02-01",
          "Mar" = "-03-01",
          "Apr" = "-04-01",
          "Mei" = "-05-01",
          "Jun" = "-06-01",
          "Jul" = "-07-01",
          "Ags" = "-08-01",
          "Sep" = "-09-01",
          "Okt" = "-10-01",
          "Nov" = "-11-01",
          "Des" = "-12-01"
        )
      )
    )
  )

# Create a `date` column by combining the `year` and `month` columns from the
# two tibbles created earlier
rsiColumnDate <- rsiColumnMonthFormatted %>%
  left_join(rsiColumnYear) %>%
  fill(year) %>%
  # Keep `date` in character type so we can use it to rename columns later
  mutate(date = paste0(year, month)) %>%
  select(date, column_name)

# Rename columns using `rsiColumnDate`
rsiColumnNamed <- rsiNoEmptyRow %>%
  slice(-c(1, 2)) %>% # Remove rows containing year and month values
  rename(deframe(rsiColumnDate))

rsiLong <- rsiColumnNamed %>%
  pivot_longer(
    cols = all_of(pull(rsiColumnDate, date)),
    names_to = "date",
    names_transform = list(date = ymd),
    values_to = "index",
    values_transform = list(index = as.double)
  )

rsiClean <- rsiLong %>%
  mutate(year = year(date)) %>%
  filter(str_detect(category_en, "TOTAL"), year >= 2017, !is.na(index)) %>%
  select(date, index) %>%
  rename("retail_sales_index_growth" = "index")

rsiClean %>%
  write_csv(here(dirYear, dirProject, "result", "retail-sales-index.csv"))


## Population ----

keyBPS <- Sys.getenv("keyBPS")

idPop <- "1886"

respPopRaw <- GET(
  "https://webapi.bps.go.id/v1/api/list",
  query = list(
    model = "data",
    domain = "0000",
    var = idPop,
    key = keyBPS
  )
)

respPopParsed <- respPopRaw %>%
  content(type = "text") %>%
  fromJSON()

respPopParsed %>% str()

anchor_regex <- function(data) {

  if (!any("val" %in% names(data))) {
    stop("`data` must contain `val` column.")
  }

  dataAnchored <- data %>%
    dplyr::mutate(val = paste0("^", val, "$"))

  return(dataAnchored)

}

idSex <- respPopParsed$turvar %>% as_tibble() %>% anchor_regex()

idProvince <- respPopParsed$vervar %>% as_tibble() %>% anchor_regex()

idYear <- respPopParsed$tahun %>% as_tibble() %>% anchor_regex()

popRaw <- as_tibble(respPopParsed$datacontent)

popLong <- popRaw %>%
  pivot_longer(
    cols = everything(),
    names_to = c("id_province", "id_composite"),
    names_sep = idPop,
    values_to = "population"
  )

provinceName <- read_csv(here(dirYear, dirProject, "data", "province-name.csv"))

provinceNameEn <- provinceName %>%
  select(-province_idn) %>%
  mutate(bps_id = paste0("^", bps_id, "$"))

regionGroup <- provinceName %>%
  select(-province_idn) %>%
  mutate(
    region = case_when(
      province_eng == "Indonesia" ~ "Indonesia",
      str_detect(bps_id, "^3") ~ "Java & Bali",
      str_detect(bps_id, "^51") ~"Java & Bali",
      TRUE ~ "Other"
    ),
    bps_id = paste0("^", bps_id, "$")
  )

popClean <- popLong %>%
  mutate(
    province = str_replace_all(id_province, deframe(provinceNameEn)),
    region = str_replace_all(id_province, deframe(regionGroup[, -2])),
    id_sex = str_sub(id_composite, 1, 3),
    sex = str_replace_all(id_sex, deframe(idSex)),
    id_year = str_sub(id_composite, 4, 6),
    year = str_replace_all(id_year, deframe(idYear))
  ) %>%
  select(province, region, sex, year, population)

popSubset <- popClean %>%
  filter(province != "Indonesia", sex == "Jumlah ", year == 2020) %>%
  select(-sex)

popWeight <- popSubset %>%
  group_by(region) %>%
  summarize(population_region = sum(population)) %>%
  ungroup() %>%
  mutate(
    population_total = sum(population_region),
    population_weight = population_region / population_total
  ) %>%
  select(region, population_weight)


## Mobility to retail and recreation places ----

urlMobility <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"

mobilityRaw <- fread(
  urlMobility,
  select = c(
    "country_region",
    "sub_region_1",
    "date",
    "retail_and_recreation_percent_change_from_baseline"
  )
)

mobilityIDN <- mobilityRaw %>%
  as_tibble() %>%
  filter(country_region == "Indonesia") %>%
  rename(
    "country" = "country_region",
    "province" = "sub_region_1",
    "mobility" = "retail_and_recreation_percent_change_from_baseline"
  ) %>%
  mutate(province = na_if(province, ""))

mobilityIDNSubset <- mobilityIDN %>%
  filter(between(date, ymd("2020-02-15"), ymd("2021-08-08")))

mobilityIDNProvRenamed <- mobilityIDNSubset %>%
  mutate(
    province = str_replace_all(
      province,
      c(
        "Special Region of " = "",
        "^South East Sulawesi$" = "Southeast Sulawesi"
      )
    )
  )


### By region ----

mobilityIDNRegion <- mobilityIDNProvRenamed %>%
  select(-country) %>%
  filter(!is.na(province)) %>%
  left_join(regionGroup[, -1], by = c("province" = "province_eng")) %>%
  relocate(province, region)

mobilityIDNRegionWeight <- mobilityIDNRegion %>% left_join(popWeight)

mobilityRegionAverageWeighted <- mobilityIDNRegionWeight %>%
  group_by(region, date) %>%
  summarize(
    mobility_region_average = mean(mobility, na.rm = TRUE),
    mobility_region_average_weighted = mobility_region_average * population_weight
  ) %>%
  ungroup()

mobilityRegionNoDupes <- mobilityRegionAverageWeighted %>%
  group_by(date) %>%
  distinct(region, .keep_all = TRUE) %>%
  ungroup()

mobilityRegionMovingAverage <- mobilityRegionNoDupes %>%
  group_by(region) %>%
  mutate(
    mobility_region_7day_moving_average = rollmean(
      mobility_region_average_weighted,
      k = 7,
      fill = NA,
      align = "right"
    )
  ) %>%
  ungroup() %>%
  select(region, date, mobility_region_7day_moving_average)

mobilityRegionMovingAverage %>%
  write_csv(here(dirYear, dirProject, "result", "mobility-retail-region.csv"))


### By province ----

mobilityProvMovingAverage <- mobilityIDNProvRenamed %>%
  filter(!is.na(province)) %>%
  select(-country) %>%
  group_by(province) %>%
  mutate(
    mobility_province_7day_moving_average = rollmean(
      mobility,
      k = 7,
      fill = NA,
      align = "right"
    )
  ) %>%
  ungroup() %>%
  filter(date == last(date)) %>%
  select(-mobility)


## COVID-19 confirmed cases ----

covidRaw <- read_csv(here(dirYear, dirProject, "data", "covid.csv"))

covidClean <- covidRaw %>%
  select(key, jumlah_kasus) %>%
  rename("cases" = "jumlah_kasus") %>%
  filter(key != "PROVINSI JAWA TENGAH") %>% # This contains a bug
  mutate(
    province = str_to_title(key),
    province = str_replace_all(
      province,
      c("Dki" = "DKI", "Daerah Istimewa" = "D.I.")
    )
  )

covidProvEn <- covidClean %>%
  left_join(provinceName, by = c("province" = "province_idn")) %>%
  select(province_eng, cases)

covidPop <- covidProvEn  %>%
  left_join(popSubset, by = c("province_eng" = "province")) %>%
  select(-c(region, year))

covidCasesPerMillion <- covidPop %>%
  mutate(
    # The population figures are in thousands
    cases_per_million_people = cases / population * 1000
  ) %>%
  select(province_eng, cases_per_million_people) %>%
  rename("province" = "province_eng")


## Mobility and COVID-19 confirmed cases per million, by province ----

mobilityCovid <- mobilityProvMovingAverage %>% left_join(covidCasesPerMillion)

mobilityCovid %>%
  write_csv(here(dirYear, dirProject, "result", "mobility-covid.csv"))
