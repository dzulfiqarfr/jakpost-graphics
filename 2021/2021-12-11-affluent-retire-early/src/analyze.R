# Packages ----

library(here)
library(tidyverse)

dirYear <- "2021"
dirProject <- "2021-12-11-affluent-retire-early"

i_am(paste(dirYear, dirProject, "src", "analyze.R", sep = "/"))


# Data ----

retirementRaw <- read_csv(
  here(
    dirYear,
    dirProject,
    "data",
    "standard-chartered-retirement-cleaned.csv"
  )
)

add_ci <- function(data, sample_size, z = 1.96) {

  sampleSize <- rlang::enquo(sample_size)

  if (!("share_of_respondents" %in% names(data))) {
    stop("`data` must contain `share_of_respondents` column.")
  }

  dataCI <- data %>%
    dplyr::mutate(
      std_err = sqrt(share_of_respondents * (100 - share_of_respondents) / !!sampleSize),
      ci_lower = share_of_respondents - 1.96 * std_err,
      ci_lower = case_when(ci_lower < 0 ~ 0, TRUE ~ ci_lower),
      ci_upper = share_of_respondents + 1.96 * std_err,
      ci_upper = case_when(ci_upper > 100 ~ 100, TRUE ~ ci_upper)
    ) %>%
    dplyr::select(-c(!!sampleSize, std_err))

  return(dataCI)

}

retireBefore65 <- retirementRaw %>%
  select(-no_retirement_savings) %>%
  rename(share_of_respondents = retire_before_65) %>%
  add_ci(sample_size = n)

retireBefore65 %>%
  write_csv(here(dirYear, dirProject, "result", "retire-early.csv"))

retirementSavings <- retirementRaw %>%
  select(-retire_before_65) %>%
  rename(share_of_respondents = no_retirement_savings) %>%
  add_ci(sample_size = n)

retirementSavings %>%
  write_csv(here(dirYear, dirProject, "result", "retirement-savings.csv"))
