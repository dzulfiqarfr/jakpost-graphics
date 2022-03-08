# Packages ----

# Set aside enough memory so we can iterate `tabulizer::extract_tables()`
# over multiple large PDFs
options(java.parameters = "-Xmx1000m")

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
library(tabulizer)
library(fs)

dirYear <- "2021"
dirProject <- "2021-07-12-wealth-gap-narrows"

i_am(paste(dirYear, dirProject, "src", "analyze.R", sep = "/"))


# Data ----

## Wealth growth between 2000 and 2020 ----

pathWealthEst <- here(
  dirYear,
  dirProject,
  "doc",
  "credit-suisse-wealth-databook-2021.pdf"
)

# Get the table areas with `tabulizer::locate_area(pathWealthEst, pages = 106)`
tableAreaWeatlhEst <- list(
  c(
    top = 53.98713,
    left = 26.58371,
    bottom = 701.83269,
    right = 565.82232
  )
)

wealthEstRaw <- extract_tables(
  pathWealthEst,
  # Choose pages containing wealth estimates for Indonesia
  pages = seq(26, 106, 4),
  area = tableAreaWeatlhEst,
  guess = FALSE,
  output = "data.frame"
)

wealthEstTibble <- wealthEstRaw %>% map_df(pluck) %>% as_tibble()

wealthEstColumnNamed <- wealthEstTibble %>%
  slice(-c(1:7)) %>% # Remove rows containing no data
  rename(
    country = 1,
    population_adult = 2,
    share_adult = 3,
    wealth_total = 4,
    wealth_share = 5,
    wealth_per_adult = 6,
    wealth_financial_per_adult = 7,
    wealth_nonfinancial_per_adult = 8,
    debt_per_adult = 9,
    wealth_median_per_adult = 10,
    estimation_method = 11
  )

wealthEstDataTypeCorrect <- wealthEstColumnNamed %>%
  mutate(
    across(.fns = ~ na_if(.x, "")),
    across(
      .cols = population_adult:wealth_median_per_adult,
      .fns = ~ str_remove_all(.x, ",")
    ),
    # Parse numeric values, which will coerce rows without any data and
    # contain scattered column names from the original data instead into `NA`s
    across(
      .cols = population_adult:wealth_median_per_adult,
      .fns = ~ parse_number(.x)
    )
  ) %>%
  # Remove those rows
  filter(!is.na(wealth_per_adult))

wealthEstYear <- wealthEstDataTypeCorrect %>%
  group_by(country) %>%
  mutate(year = seq(2000, 2020)) %>%
  ungroup() %>%
  relocate(country, year)

wealthEstClean <- wealthEstYear %>% filter(country == "Indonesia")

# Define a function to calculate the contribution to growth
weight_growth <- function(data, col, colName) {

  col <- rlang::enquo(col)
  colName <- rlang::enquo(colName)

  if (!any("wealth_per_adult" %in% names(data))) {
    stop("`data` must contain `wealth_per_adult` column.")
  }

  dataWeightedGrowth <- data %>%
    dplyr::mutate(
      diff = !!col - dplyr::lag(!!col),
      growth = diff / dplyr::lag(!!col) * 100,
      weight = !!col / wealth_per_adult,
      !!colName := growth * dplyr::lag(weight)
    ) %>%
    dplyr::select(-c(diff, growth, weight))

  return(dataWeightedGrowth)

}

wealthWeightedGrowth <- wealthEstClean %>%
  weight_growth(wealth_financial_per_adult, "financial_weighted_growth") %>%
  weight_growth(
    wealth_nonfinancial_per_adult,
    "nonfinancial_weighted_growth"
  ) %>%
  weight_growth(debt_per_adult, "debt_weighted_growth") %>%
  # Multiply the `debt_weighted_growth` by -1 as it negatively contributed to
  # the wealth growth
  mutate(debt_weighted_growth = debt_weighted_growth * -1) %>%
  select(country, year, contains("weighted_growth"))

wealthWeightedGrowthClean <- wealthWeightedGrowth %>%
  pivot_longer(
    cols = contains("weighted_growth"),
    names_to = "category",
    values_to = "weighted_growth"
  ) %>%
  mutate(
    category = str_replace_all(
      category,
      c(
        "^financial_weighted_growth$" = "Financial asset",
        "nonfinancial_weighted_growth" = "Nonfinancial asset",
        "debt_weighted_growth" = "Debt"
      )
    )
  ) %>%
  filter(!is.na(weighted_growth))

wealthWeightedGrowthClean %>%
  write_csv(
    here(
      dirYear,
      dirProject,
      "result",
      "wealth-contribution-growth.csv"
    )
  )


## Wealth distribution ----

lsWealthEst <- dir_ls(here(dirYear, dirProject, "doc"))

# Get the table areas using the `tabulizer::locate_area()` again, respectively
tableAreasWealthDis <- list(
  databook2019 = list(
    c(
      top = 53.35751,
      left = 35.84341,
      bottom = 544.34924,
      right = 559.47660
    )
  ),
  databook2021 = list(
    c(
      top = 57.97499,
      left = 32.70321,
      bottom = 539.73176,
      right = 559.41660
    )
  )
)

argsExtractWealthDis <- list(
  file = as.list(lsWealthEst),
  pages = list(168, 136),
  area = tableAreasWealthDis
)

wealthDisRaw <- argsExtractWealthDis %>%
  pmap(
    extract_tables,
    guess = FALSE,
    output = "data.frame"
  ) %>%
  set_names(c("2019", "2020"))

wealthDisTibble <- wealthDisRaw %>% map(pluck, 1) %>% map(as_tibble)

# Separate the `Wealth.decile` column because it contains data for decile 4
# and 5, which accidentally get merged when extracted from the PDF
wealthDisColumnCorrect <- wealthDisTibble %>%
  map(
    function(data)
      separate(
        data,
        col = Wealth.decile,
        into = c("decile_4", "decile_5"),
        sep = "\\s"
      )
  )

wealthDisColumnNamed <- wealthDisColumnCorrect %>%
  map(
    function(data)
      rename(
        data,
        country = 1,
        decile_1 = 2,
        decile_2 = 3,
        decile_3 = 4,
        decile_6 = 7,
        decile_7 = 8,
        decile_8 = 9,
        decile_9 = 10,
        top_10 = 11,
        top_5 = 12,
        top_1 = 13
      )
  )

wealthDisDataTypeCorrect <- wealthDisColumnNamed %>%
  map(slice, -c(1:2)) %>%
  map(
    function(data)
      data %>%
      mutate(across(.cols = decile_1:top_1, .fns = ~ as.double(.x)))
  )

wealthDisYear <- wealthDisDataTypeCorrect %>% bind_rows(.id = "year")

wealthDisClean <- wealthDisYear %>%
  filter(country == "Indonesia") %>%
  pivot_longer(
    cols = decile_1:top_1,
    names_to = "category",
    values_to = "wealth_distribution"
  ) %>%
  mutate(category = str_replace_all(category, "_", " "))

wealthDisSubset <- wealthDisClean %>%
  filter(!(category %in% c("top 5", "top 1")))

wealthDisCumulative <- wealthDisSubset %>%
  group_by(year) %>%
  mutate(
    category = str_replace_all(category, "top", "decile"),
    wealth_distribution_cumulative = cumsum(wealth_distribution),
    # Round the numbers so they do not exceed 100
    wealth_distribution_cumulative = case_when(
      wealth_distribution_cumulative > 100 ~ 100,
      TRUE ~ wealth_distribution_cumulative
    )
  ) %>%
  ungroup()

wealthDisCumulative %>%
  write_csv(here(dirYear, dirProject, "result", "wealth-distribution.csv"))
