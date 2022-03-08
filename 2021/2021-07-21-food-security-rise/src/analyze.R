# Packages ----

library(conflicted)
library(here)
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
  set_names(c("2018", "2020"))

foodSecurityMerged <- foodSecurityRaw %>% bind_rows(.id = "year")

foodSecurityClean <- foodSecurityMerged %>%
  select_if(function(x) !all(is.na(x))) %>%
  separate(
    col = Wilayah,
    into = c("province", "city"),
    sep = "\\s-\\s"
  ) %>%
  rename(food_security_index = IKP)

foodSecurityClean %>%
  write_csv(here(dirYear, dirProject, "result", "food-security.csv"))
