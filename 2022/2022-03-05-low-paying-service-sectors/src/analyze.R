dirYear <- "2022"
dirProject <- "2022-03-05-low-paying-service-sectors"

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

idLabor <- "1971"

respLaborRaw <- GET(
  "https://webapi.bps.go.id/v1/api/list",
  query = list(
    model = "data",
    domain = "0000",
    var = idLabor,
    key = keyBPS
  )
)

respLaborParsed <- respLaborRaw %>%
  content(type = "text") %>%
  fromJSON()

anchor_regex <- function(df, col = val) {

  col <- rlang::enquo(col)

  dfAnchored <- df %>% dplyr::mutate(!!col := paste0("^", !!col, "$"))

  return(dfAnchored)

}

idEducation <- respLaborParsed$turvar %>% as_tibble() %>% anchor_regex()

idSector <- respLaborParsed$vervar %>% as_tibble() %>% anchor_regex()

idYear <- respLaborParsed$tahun %>% as_tibble() %>% anchor_regex()

laborRaw <- as_tibble(respLaborParsed$datacontent)

laborLong <- laborRaw %>%
  pivot_longer(
    cols = everything(),
    names_to = c("id_sector", "id_composite"),
    names_sep = idLabor,
    values_to = "worker"
  )

idSector <- read_csv(here(dirYear, dirProject, "data", "isic.csv"))

idSector <- anchor_regex(idSector, col = id_bps)

laborClean <- laborLong %>%
  mutate(
    sector = str_replace_all(id_sector, deframe(idSector[, -3])),
    id_education = str_sub(id_composite, 1, 3),
    education = str_replace_all(id_education, deframe(idEducation)),
    id_year = str_sub(id_composite, 4, 6),
    year = str_replace_all(id_year, deframe(idYear)),
    id_month = str_sub(id_composite, 7, 9),
    month = str_replace_all(id_month, c("189" = "-02-01", "190" = "-08-01")),
    date = as.Date(paste0(year, month)),
    across(.cols = -worker, .fns = ~ str_trim(.x))
  ) %>%
  select(sector, id_education, education, date, worker)

laborClassified <- laborClean %>%
  left_join(idSector[, -1], by = "sector") %>%
  relocate(sector, classification)

laborServiceDist <- laborClassified %>%
  filter(
    date == as.Date("2021-08-01"),
    classification == "Services",
    education == "Total"
  ) %>%
  mutate(
    worker_total = sum(worker),
    worker_share = worker / worker_total * 100
  ) %>%
  select(sector, date, worker_share)

laborServiceDist %>%
  write_csv(
    here(
      dirYear,
      dirProject,
      "result",
      "labor-service-sector-distribution-overall.csv"
    )
  )

laborEduGroupReduced <- laborClassified %>%
  filter(
    date == as.Date("2021-08-01"),
    classification == "Services",
    education != "Total"
  ) %>%
  mutate(
    education = case_when(
      id_education == "818" ~ "None",
      id_education %in% c("819", "820") ~ "Elementary school or lower",
      id_education == "821" ~ "Junior high school",
      id_education %in% c("822", "823") ~ "Senior high school",
      id_education == "824"  ~ "Vocational school",
      id_education == "825" ~ "University"
    )
  )

laborServiceEduDist <- laborEduGroupReduced %>%
  group_by(sector, education) %>%
  summarize(worker = sum(worker)) %>%
  ungroup(education) %>%
  mutate(
    worker_total_sector = sum(worker),
    worker_share_edu = worker / worker_total_sector * 100
  ) %>%
  ungroup() %>%
  mutate(date = as.Date("2021-08-01")) %>%
  select(sector, education, date, worker_share_edu)

laborServiceEduDist %>%
  write_csv(
    here(
      dirYear,
      dirProject,
      "result",
      "labor-service-sector-distribution-by-education.csv"
    )
  )
