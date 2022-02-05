# Packages ----

library(conflicted)
library(here)
conflict_prefer("here", "here")
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(httr)
library(jsonlite)

dirYear <- "2021"
dirProject <- "2021-05-23-indonesia-remains-java-centric"

i_am(paste(dirYear, dirProject, "src", "analyze.R", sep = "/"))


# Data ----

## Population ----

pop2010 <- read_csv(
  here(dirYear, dirProject, "data", "bps-population-2010-cleaned.csv"),
  na = "-"
)

pop2020 <- read_csv(
  here(dirYear, dirProject, "data", "bps-population-2020-cleaned.csv")
)

pop2020total <- pop2020 %>%
  select(province, total) %>%
  rename("2020" = "total")

popAll <- pop2010 %>%
  left_join(pop2020total) %>%
  pivot_longer(
    cols = 2:ncol(.),
    names_to = "year",
    values_to = "population"
  )

popTotal <- popAll %>%
  filter(province == "INDONESIA") %>%
  select(-province) %>%
  rename("total" = "population")

popAllTotal <- popAll %>%
  filter(province != "INDONESIA") %>%
  left_join(popTotal)

# Create island groups to find the population distribution
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

groupBaliNT <- c(
  "Bali",
  "Nusa Tenggara Timur",
  "Nusa Tenggara Barat"
)

groupMalukuPapua <- c(
  "Maluku Utara",
  "Maluku",
  "Papua",
  "Papua Barat"
)

add_island_group <- function(data) {

  if (!any("province" %in% names(data))) {
    stop("`data` must contain `province` column.")
  }

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

popIsland <- add_island_group(popAllTotal)

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

urlBPS <- "https://webapi.bps.go.id/v1/api/list"

idVar <- "533"

respGRDPraw <- GET(
  url = urlBPS,
  query = list(model = "data", domain = "0000", var = idVar, key = keyBPS)
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

GRDPraw <- as_tibble(respGRDPparsed$datacontent)

GRDPclean <- GRDPraw %>%
  pivot_longer(
    cols = everything(),
    names_to = "id_composite",
    values_to = "expenditure"
  ) %>%
  separate(
    col = id_composite,
    into = c("id_province", "id_composite"),
    sep = idVar
  ) %>%
  mutate(
    # Get length of `id_composite` to find the ones inadvertently cut as the
    # variable id, namely 533, coincides with a year + quarter IDs for
    # observations in the July to September period of 2015
    len_composite = str_length(id_composite),
    id_composite = case_when(
      near(len_composite, 6) ~ paste0(id_composite, idVar),
      TRUE ~ id_composite
    ),
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

GRDPannual <- GRDPclean %>%
  filter(
    component == "PDRB",
    quarter == "Tahunan",
    !str_detect(province, "34")
  ) %>%
  mutate(
    province = str_replace_all(province, "KEP\\.", "Kepulauan"),
    province = str_to_title(province),
    province = str_replace_all(province, c("Dki" = "DKI", "Di" = "DI"))
  ) %>%
  select(-c(component, quarter)) %>%
  rename("grdp" = "expenditure")

GRDPisland <- GRDPannual %>%
  add_island_group() %>%
  group_by(year, island) %>%
  mutate(grdp_island = sum(grdp, na.rm = TRUE)) %>%
  ungroup(island) %>%
  select(-c(province, grdp)) %>%
  distinct(island, .keep_all = TRUE) %>%
  ungroup() %>%
  relocate(island)

GDP <- GRDPclean %>%
  filter(
    component == "PDRB",
    quarter == "Tahunan",
    str_detect(province, "34")
  ) %>%
  select(year, expenditure) %>%
  rename("total" = "expenditure")

GDPdistribution <- GRDPisland %>%
  left_join(GDP) %>%
  mutate(gdp_share = grdp_island / total * 100) %>%
  select(-c(grdp_island, total))

GDPdistribution %>%
  write_csv(here(dirYear, dirProject, "result", "gdp-distribution.csv"))


## Lifetime migration ----

migration <- read_csv(
  here(dirYear, dirProject, "data", "bps-lifetime-migration-cleaned.csv")
)

migrationIn <- migration %>%
  select(province, in_share) %>%
  mutate(province = str_replace_all(province, "Kep\\.", "Kepulauan"))


## Incoming lifetime migrants and GRDP ----

GRDPmigration <- GRDPannual %>%
  filter(year == "2020") %>%
  left_join(migrationIn) %>%
  left_join(pop2020total) %>%
  rename("population" = "2020", "migrant_incoming_share" = "in_share") %>%
  add_island_group()

GRDPperCapita <- GRDPmigration %>%
  mutate(grdp_per_capita = grdp / population) %>%
  select(
    province,
    island,
    year,
    population,
    grdp,
    migrant_incoming_share,
    grdp_per_capita
  )

GRDPperCapita %>%
  write_csv(here(dirYear, dirProject, "result", "migration-grdp.csv"))
