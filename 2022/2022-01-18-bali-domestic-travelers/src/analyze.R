dirYear <- "2022"
dirProject <- "2022-01-18-bali-domestic-travelers"

here::i_am(paste(dirYear, dirProject, "src", "analyze.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(httr)
library(jsonlite)


# Data ----

keyBPS <- Sys.getenv("keyBPS")

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
    httr::content(type = "text") %>%
    jsonlite::fromJSON()

  return(respParsed)

}

bps_tidy <- function(data,
                     idVar,
                     idProvince,
                     idYear,
                     category,
                     subcategory) {

  if (nrow(data) != 1) {
    stop("`data` must be wide and has one row.")
  }

  if (!all(c("bps_id", "province_eng") %in% names(idProvince))) {
    stop("`idProvince` must contain `bps_id` and `province_eng` columns.")
  }

  stopifnot(
    is.character("idVar"),
    is.character("category"),
    is.character("subcategory")
  )

  guestColumnName <- paste0(category, "_", subcategory, "_guest")

  dataLong <- data %>%
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = c("id_province", "id_year"),
      names_sep = idVar,
      values_to = guestColumnName
    )

  dataClean <- dataLong %>%
    dplyr::mutate(
      province = stringr::str_replace_all(
        id_province,
        tibble::deframe(idProvince)
      ),
      # Remove leading 0s, which mean empty `turvar` in the API response
      id_year = stringr::str_remove_all(id_year, '^0'),
      # Remove trailing 0s, which mean empty `turtahun`, ie month/quarter/period
      id_year = stringr::str_remove_all(id_year, '0$'),
      year = stringr::str_replace_all(id_year, tibble::deframe(idYear)),
      year = as.integer(year)
    ) %>%
    dplyr::select(province, year, rlang::sym(guestColumnName))

  return(dataClean)

}

argsIdVar <- list(
  idVar = c(
    "328", # Domestic guests at star hotels
    "329", # Domestic guests at nonstar hotels
    "310", # Foreign guests at star hotels
    "326" # Foreign guests at nonstar hotels
  )
)

respGuest <- pmap(argsIdVar, bps_request, key = keyBPS)

respGuestNamed <- respGuest %>%
  set_names(
    c(
      "domestic_star",
      "domestic_nonstar",
      "foreign_star",
      "foreign_nonstar"
    )
  )

# Create a tibble containing year id from one of the data
idYear <- respGuestNamed %>%
  pluck("domestic_star", "tahun") %>%
  as_tibble() %>%
  mutate(val = paste0("^", val, "$"))

province <- read_csv(here(dirYear, dirProject, "data", "province.csv"))

provinceEn <- province %>%
  select(-province_idn) %>%
  mutate(bps_id = paste0("^", bps_id, "$"))

# Create a tibble with a three-digit `bps_id` to clean foreign guests at star
# hotels data later
provinceEn3dig <- province %>%
  select(-province_idn) %>%
  mutate(
    bps_id = str_sub(bps_id, 1, 3),
    bps_id = paste0("^", bps_id, "$")
  )

guestRaw <- respGuestNamed %>%
  map(
    function(data)
      data %>%
      pluck("datacontent") %>%
      as_tibble()
  )

# Add `\\d` to the id of foreign guests at star hotels data to prevent a bug
# when separating the composite id into columns of province and year id.
# The bug will otherwise occur because the data id is the same with
# the first three digits of Jakarta's province id, namely `3100`
argsGuest <- list(
  data = guestRaw,
  idVar = list("328", "329", "\\d310", "326"),
  idProvince = list(provinceEn, provinceEn, provinceEn3dig, provinceEn),
  category = list("domestic", "domestic", "foreign", "foreign"),
  subcategory = list("star", "nonstar", "star", "nonstar")
)

guestTidy <- pmap(argsGuest, bps_tidy, idYear = idYear)

guestSubset <- guestTidy %>%
  map(~ filter(.x, province != "Indonesia", year == 2019))

guestMerged <- guestSubset[["domestic_star"]] %>%
  left_join(guestSubset[["domestic_nonstar"]]) %>%
  left_join(guestSubset[["foreign_star"]]) %>%
  left_join(guestSubset[["foreign_nonstar"]])

guestForeignShare <- guestMerged %>%
  mutate(
    domestic_guest_total = domestic_star_guest + domestic_nonstar_guest,
    foreign_guest_total = foreign_star_guest + foreign_nonstar_guest,
    overall_guest_total = domestic_guest_total + foreign_guest_total,
    foreign_guest_share = foreign_guest_total / overall_guest_total * 100
  ) %>%
  select(province, year, foreign_guest_share)

guestForeignShare %>%
  write_csv(here(dirYear, dirProject, "result", "foreign-guest-share.csv"))
