# Packages ----

library(conflicted)
library(here)
conflict_prefer("here", "here")
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(fs)


dirYear <- "2021"
dirProject <- "2021-07-21-food-security-rise"

i_am(paste(dirYear, dirProject, "src", "analyze.R", sep = "/"))


# Data ----

lsFoodSecurity <- dir_ls(here(dirYear, dirProject, "data"))

foodSecurityRaw <- lsFoodSecurity %>%
  map(read_csv) %>%
  set_names(c("2018", "2019", "2020"))

foodSecurityMerged <- foodSecurityRaw %>% bind_rows(.id = "year")

foodSecurityClean <- foodSecurityMerged %>%
  select_if(function(x) !all(is.na(x))) %>%
  separate(
    col = Wilayah,
    into = c("province", "city"),
    sep = "\\s-\\s"
  ) %>%
  rename(
    "composite" = "Komposit",
    "ncpr" = "NCPR",
    "poverty_rate" = "Kemiskinan (%)",
    "food_expenditure_share" = "Pengeluaran Pangan (%)",
    "no_electricity_population_share" = "Tanpa Listrik (%)",
    "no_water_population_share" = "Tanpa Air Bersih (%)",
    "mean_years_schooling_women" = "Lama Sekolah Perempuan (tahun)",
    "health_worker_population_share" = "Rasio Tenaga Kesehatan",
    "life_expectancy" = "Angka Harapan Hidup (tahun)",
    "stunting" = "Stunting (%)",
    "food_security_index" = "IKP",
    "food_security_index_ranking" = "IKP Ranking"
  )

foodSecurityClean %>%
  write_csv(here(dirYear, dirProject, "result", "food-security.csv"))
