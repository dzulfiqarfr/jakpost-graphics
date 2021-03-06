dirYear <- "2021"
dirProject <- "2021-05-23-indonesia-remains-java-centric"

here::i_am(paste(dirYear, dirProject, "src", "analyze.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(httr)
library(jsonlite)


# Data ----

## Population ----

pop2010 <- read_csv(
  here(dirYear, dirProject, "data", "bps-population-2010-cleaned.csv"),
  na = "-"
)

pop2020 <- read_csv(
  here(
    dirYear,
    dirProject,
    "data",
    "bps-population-2020-cleaned.csv"
  )
)

pop2020total <- pop2020 %>%
  select(province, total) %>%
  rename(`2020` = total)

popAll <- pop2010 %>%
  left_join(pop2020total) %>%
  pivot_longer(
    cols = 2:ncol(.),
    names_to = "year",
    values_to = "population"
  )

popNational <- popAll %>%
  filter(province == "INDONESIA") %>%
  select(-province) %>%
  rename(total = population)

popAllNational <- popAll %>%
  filter(province != "INDONESIA") %>%
  left_join(popNational)

add_island_group <- function(data) {

  if (!any("province" %in% names(data))) {
    stop("`data` must contain `province` column.")
  }

  groupJava <- c(
    "DKI Jakarta",
    "Jawa Tengah",
    "Jawa Timur",
    "Jawa Barat",
    "DI Yogyakarta",
    "Banten"
  )

  groupKalimantan <- c(
    "Kalimantan Tengah",
    "Kalimantan Timur",
    "Kalimantan Barat",
    "Kalimantan Selatan",
    "Kalimantan Utara"
  )

  groupSulawesi <- c(
    "Sulawesi Tengah",
    "Sulawesi Tenggara",
    "Sulawesi Barat",
    "Sulawesi Utara",
    "Sulawesi Selatan",
    "Gorontalo"
  )

  groupSumatra <- c(
    "Aceh",
    "Sumatera Barat",
    "Jambi",
    "Bengkulu",
    "Kepulauan Bangka Belitung",
    "Sumatera Utara",
    "Riau",
    "Sumatera Selatan",
    "Lampung",
    "Kepulauan Riau"
  )

  groupBaliNT <- c("Bali", "Nusa Tenggara Timur", "Nusa Tenggara Barat")

  groupMalukuPapua <- c(
    "Maluku Utara",
    "Maluku",
    "Papua",
    "Papua Barat"
  )

  dataIslandGrouped <- data %>%
    dplyr::mutate(
      island = dplyr::case_when(
        province %in% groupJava ~ "Java",
        province %in% groupSumatra ~ "Sumatra",
        province %in% groupSulawesi ~ "Sulawesi",
        province %in% groupKalimantan ~ "Kalimantan",
        province %in% groupBaliNT ~ "Bali & Nusa Tenggara",
        TRUE ~ "Maluku & Papua"
      )
    ) %>%
    dplyr::relocate(province, island)

  return(dataIslandGrouped)

}

popIsland <- add_island_group(popAllNational)

popDistribution <- popIsland %>%
  group_by(year, island) %>%
  mutate(population_island = sum(population, na.rm = TRUE)) %>%
  ungroup(island) %>%
  select(-c(province, population)) %>%
  distinct(island, .keep_all = TRUE) %>%
  ungroup() %>%
  mutate(population_share = population_island / total * 100) %>%
  select(-c(total, population_island)) %>%
  relocate(island)

popDistribution %>%
  write_csv(here(dirYear, dirProject, "result", "population-distribution.csv"))


## Gross regional domestic product ----

keyBPS <- Sys.getenv("keyBPS")

idGRDP <- "533"

respGRDPraw <- GET(
  "https://webapi.bps.go.id/v1/api/list",
  query = list(
    model = "data",
    domain = "0000",
    var = idGRDP,
    key = keyBPS
  )
)

respGRDPparsed <- respGRDPraw %>%
  content(type = "text") %>%
  fromJSON()

anchor_regex <- function(data) {

  if (!any("val" %in% names(data))) {
    stop("`data` must contain `val` column.")
  }

  dataAnchored <- data %>%
    dplyr::mutate(val = paste0("^", val, "$"))

  return(dataAnchored)

}

idComponent <- respGRDPparsed$turvar %>% as_tibble() %>% anchor_regex()

idProvince <- respGRDPparsed$vervar %>% as_tibble() %>% anchor_regex()

idYear <- respGRDPparsed$tahun %>% as_tibble() %>% anchor_regex()

idQuarter <- respGRDPparsed$turtahun %>% as_tibble() %>% anchor_regex()

grdpRaw <- as_tibble(respGRDPparsed$datacontent)

grdpLong <- grdpRaw %>%
  pivot_longer(
    cols = everything(),
    names_to = c("id_province", "id_composite"),
    names_sep = idGRDP,
    values_to = "expenditure"
  )

# Restore `id_composite` that has been inadvertently cut when separating it
# into two columns earlier because the variable id, 533, is the same with a
# combination of year and quarter ids for observations in the July to
# September period in 2015
grdpIdCompRestored <- grdpLong %>%
  mutate(
    len_composite = str_length(id_composite),
    id_composite = case_when(
      near(len_composite, 6) ~ paste0(id_composite, idGRDP),
      TRUE ~ id_composite
    )
  )

grdpClean <- grdpIdCompRestored %>%
  mutate(
    id_component = case_when(
      str_detect(id_composite, "^6") ~ str_sub(id_composite, 1, 3),
      TRUE ~ str_sub(id_composite, 1, 4)
    ),
    id_year = case_when(
      str_detect(id_composite, "^6") ~ str_sub(id_composite, 4, 6),
      TRUE ~ str_sub(id_composite, 5, 7)
    ),
    id_quarter = case_when(
      str_detect(id_composite, "^6") ~ str_sub(id_composite, 7, 8),
      TRUE ~ str_sub(id_composite, 8, 9)
    ),
    province = str_replace_all(id_province, deframe(idProvince)),
    component = str_replace_all(id_component, deframe(idComponent)),
    year = str_replace_all(id_year, deframe(idYear)),
    quarter = str_replace_all(id_quarter, deframe(idQuarter))
  ) %>%
  select(province, component, year, quarter, expenditure)

grdpAnnual <- grdpClean %>%
  filter(
    component == "PDRB",
    quarter == "Tahunan",
    year != 2021,
    !str_detect(province, "34")
  ) %>%
  # Ensure consistent province names to allow island grouping with
  # `add_island_group()` later
  mutate(
    province = str_replace_all(province, "KEP\\.", "Kepulauan"),
    province = str_to_title(province),
    province = str_replace_all(province, c("Dki" = "DKI", "Di" = "DI"))
  ) %>%
  select(-c(component, quarter)) %>%
  rename(grdp = expenditure)

grdpIsland <- grdpAnnual %>%
  add_island_group() %>%
  group_by(year, island) %>%
  mutate(grdp_island = sum(grdp, na.rm = TRUE)) %>%
  ungroup(island) %>%
  select(-c(province, grdp)) %>%
  distinct(island, .keep_all = TRUE) %>%
  ungroup() %>%
  relocate(island)

gdpNational <- grdpClean %>%
  filter(
    component == "PDRB",
    quarter == "Tahunan",
    str_detect(province, "34")
  ) %>%
  select(year, expenditure) %>%
  rename(total = expenditure)

gdpDistribution <- grdpIsland %>%
  left_join(gdpNational) %>%
  mutate(gdp_share = grdp_island / total * 100) %>%
  select(-c(grdp_island, total))

gdpDistribution %>%
  write_csv(here(dirYear, dirProject, "result", "gdp-distribution.csv"))


## Lifetime migration ----

migration <- read_csv(
  here(
    dirYear,
    dirProject,
    "data",
    "bps-lifetime-migration-cleaned.csv"
  )
)

migrationIn <- migration %>%
  select(province, in_share) %>%
  mutate(province = str_replace_all(province, "Kep\\.", "Kepulauan"))


## Incoming lifetime migrants and GRDP ----

grdpPerCap <- grdpAnnual %>%
  filter(year == 2020) %>%
  left_join(pop2020total) %>%
  rename(population = `2020`) %>%
  mutate(grdp_per_cap = grdp / population) # In million rupiah

grdpMigrationIn <- grdpPerCap %>%
  left_join(migrationIn) %>%
  rename(migrant_incoming_share = in_share) %>%
  add_island_group() %>%
  select(
    province,
    island,
    year,
    population,
    grdp,
    migrant_incoming_share,
    grdp_per_cap
  )

grdpMigrationIn %>%
  write_csv(here(dirYear, dirProject, "result", "migration-grdp.csv"))
