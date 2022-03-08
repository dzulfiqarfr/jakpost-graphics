# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(httr)
library(jsonlite)
library(readxl)

dirYear <- "2021"
dirProject <- "2021-06-28-homeownership-slides"

i_am(paste(dirYear, dirProject, "src", "analyze.R", sep = "/"))


# Data ----

## Homeownership ----

keyBPS <- Sys.getenv("keyBPS")

idOwnership <- "849"

respOwnershipRaw <- GET(
  "https://webapi.bps.go.id/v1/api/list",
  query = list(
    model = "data",
    domain = "0000",
    var = idOwnership,
    key = keyBPS
  )
)

respOwnershipParsed <- respOwnershipRaw %>%
  content(type = "text") %>%
  fromJSON()

idYear <- respOwnershipParsed %>%
  .[["tahun"]] %>%
  as_tibble() %>%
  mutate(val = paste0("^", val, "$"))

ownershipRaw <- as_tibble(respOwnershipParsed$datacontent)

ownershipLong <- ownershipRaw %>%
  pivot_longer(
    cols = everything(),
    names_to = c("id_province", "id_date"),
    names_sep = idOwnership,
    values_to = "homeownership"
  )

provinceName <- read_csv(here(dirYear, dirProject, "data", "province.csv"))

provinceNameEn <- provinceName %>%
  select(-province_idn) %>%
  mutate(bps_id = paste0("^", bps_id, "$"))

ownershipClean <- ownershipLong %>%
  mutate(
    province = str_replace_all(id_province, deframe(provinceNameEn)),
    # Remove leading 0s in `id_date`, which means empty `turvar`
    id_date = str_remove_all(id_date, "^0"),
    # Also remove trailing 0s in `id_date`, which means empty `turtahun`
    id_date = str_remove_all(id_date, "0$"),
    year = str_replace_all(id_date, deframe(idYear)),
    year = as.integer(year)
  ) %>%
  select(province, year, homeownership)

# Subset provinces with the highest and lowest ownership, as well as the
# national figure. Then index the data to better see the trend
ownershipIndex <- ownershipClean %>%
  filter(
    province %in% c("Bengkulu", "Jambi", "Bali", "Jakarta", "Indonesia")
  ) %>%
  group_by(province) %>%
  filter(between(year, 1999, 2020)) %>%
  mutate(homeownership_index = homeownership / first(homeownership) * 100) %>%
  ungroup() %>%
  select(-homeownership)

ownershipIndex %>%
  write_csv(here(dirYear, dirProject, "result", "homeownership-index.csv"))

ownershipBottom10 <- ownershipClean %>%
  filter(year == 2020) %>%
  arrange(homeownership) %>%
  head(10)

ownershipBottom10 %>%
  write_csv(here(dirYear, dirProject, "result", "homeownership-bottom-10.csv"))


## House price index ----

priceRaw <- read_excel(
  here(dirYear, dirProject, "data", "bi-shpr-raw.xlsx"),
  sheet = "TABEL 2",
  na = c("", "-")
)

priceNoEmptyRow <- priceRaw %>%
  select(-c(1, 3)) %>%  # Remove row number and joined city-house-type columns
  slice(-c(1:3)) %>%  # Remove the first three empty rows
  rename("city" = "...2", "house_type" = "...4") %>%
  filter(!is.na(...5)) # Remove empty rows between data

priceCityFilled <- priceNoEmptyRow %>%
  mutate(
    city = case_when(
      city == "(Termasuk Jabodebek" | city == "dan Banten)" ~ "GABUNGAN 16 KOTA",
      TRUE ~ city
    )
  ) %>%
  fill(city)

# Create a tibble containing column names and year, which is stored in the
# first row
columnYear <- priceCityFilled %>%
  slice(1) %>%
  select(-c(city, house_type)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "column_name",
    values_to = "year"
  ) %>%
  mutate(year = parse_number(year)) %>%
  filter(!is.na(year))

# Also create another tibble containing column names and quarter or period,
# which is stored in the second row
columnQuarter <- priceCityFilled %>%
  slice(2) %>%
  select(-c(city, house_type)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "column_name",
    values_to = "quarter"
  ) %>%
  filter(!is.na(quarter)) %>%
  mutate(
    quarter = str_remove_all(quarter, "\\s"),
    quarter = str_remove_all(quarter, "[:punct:]"),
    date = case_when(
      quarter == "QI" ~ "-01-01",
      quarter == "QII" ~ "-04-01",
      quarter == "QIII" ~ "-07-01",
      quarter == "QIV" ~ "-10-01"
    )
  )

# Join the year and quarter tibbles to construct a date column
columnDate <- columnQuarter %>%
  left_join(columnYear, by = "column_name") %>%
  fill(year) %>%
  mutate(date = as.Date(paste0(year, date))) %>%
  select(date, column_name)

# Add a growth category. This will help avoid duplicated column names when we
# use the date tibble to replace the `...*` column names later
growthCategory <- columnDate %>%
  filter(date == first(date)) %>%
  mutate(
    growth =
      case_when(
        column_name == first(column_name) ~ "quarter-on-quarter",
        TRUE ~ "year-on-year"
      )
  )

# Join the date and growth category tibbles
columnNameReplacement <- columnDate %>%
  left_join(growthCategory, by = c("date", "column_name")) %>%
  fill(growth) %>%
  mutate(replacement = paste0(date, "_", growth)) %>%
  select(replacement, column_name)

# Use the date with growth category tibble to rename the columns
priceColumnNamed <- priceCityFilled %>%
  rename(deframe(columnNameReplacement)) %>%
  slice(-c(1:2)) # Remove rows containing year and quarter

priceLong <- priceColumnNamed %>%
  fill(city) %>%
  pivot_longer(
    cols = pull(columnNameReplacement, replacement),
    names_to = c("date", "growth"),
    names_sep = "_",
    values_to = "house_price_index_growth"
  )

houseTypeEn <- c("Kecil" = "Small", "Menengah" = "Medium", "Besar" = "Large")

priceClean <- priceLong %>%
  mutate(
    city = str_to_title(city),
    city = str_replace_all(city, "Gabungan 16 Kota", "Overall"),
    house_type = str_to_sentence(house_type),
    house_type = str_replace_all(house_type, houseTypeEn),
    house_price_index_growth = str_remove_all(
      house_price_index_growth, "\\s\\(r\\)"
    ),
    house_price_index_growth = str_replace_all(
      house_price_index_growth, ",", "."
    ),
    house_price_index_growth = as.double(house_price_index_growth)
  ) %>%
  select(city, house_type, date, growth, house_price_index_growth)

priceSubset <- priceClean %>%
  filter(
    city %in% c("Manado", "Jabodebek-Banten", "Bandar Lampung", "Overall"),
    house_type == "Total",
    growth == "year-on-year"
  )

priceSubset %>%
  write_csv(here(dirYear, dirProject, "result", "house-price-index.csv"))


## Mortgage ----

mortgagePayment <- read_csv(
  here(
    dirYear,
    dirProject,
    "data",
    "bps-mortgage-payment-2019-cleaned.csv"
  )
)

mortgagePaymentOverall <- mortgagePayment %>%
  select(province, overall) %>%
  rename("payment" = "overall")

mortgageTerm <- read_csv(
  here(
    dirYear,
    dirProject,
    "data",
    "bps-mortgage-term-2019-cleaned.csv"
  )
)

mortgageTermOverall <- mortgageTerm %>%
  select(province, overall) %>%
  rename("term" = "overall")

mortgageOverall <- mortgagePaymentOverall %>%
  left_join(mortgageTermOverall, by = "province")

mortgageOverall %>%
  write_csv(here(dirYear, dirProject, "result", "mortgage.csv"))
