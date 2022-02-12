# Packages ----

library(conflicted)
library(here)
conflict_prefer("here", "here")
library(tidyverse)
conflict_prefer("lag", "dplyr")
conflict_prefer("filter", "dplyr")
library(readxl)
library(lubridate)

dirYear <- "2021"
dirProject <- "2021-06-30-imports-suggest-uncertain-recovery"

i_am(paste(dirYear, dirProject, "src", "analyze.R", sep = "/"))


# Data ----

importRaw <- read_excel(
  here(dirYear, dirProject, "data", "bi-import-raw.xls"),
  sheet = "5.19",
  na = ""
)

importNoEmptyRows <- importRaw %>%
  select(-c(1, ncol(.))) %>%  # Remove column containing row numbers
  select_if(function(x) !all(is.na(x))) %>%
  filter(!is.na((...4))) %>% # Remove empty rows
  rename("category_idn" = "...3", "category_eng" = "...154")

# Create a tibble containing column names and month, which is stored in the
# second row
columnMonth <- importNoEmptyRows %>%
  select(-starts_with("category")) %>%
  slice(2) %>%
  pivot_longer(
    cols = everything(),
    names_to = "column_name",
    values_to = "month",
    values_transform = list(month = as.character)
  ) %>%
  mutate(
    month = str_remove_all(month, "[:punct:]"),
    month = case_when(
      is.na(month) & lag(month) == "Dec" ~ "annual",
      TRUE ~ month
    ),
    month = str_replace_all(
      month,
      c(
        "Jan" = "-01-01",
        "Feb" = "-02-01",
        "Mar" = "-03-01",
        "Apr" = "-04-01",
        "May" = "-05-01",
        "Jun" = "-06-01",
        "Jul" = "-07-01",
        "Aug" = "-08-01",
        "Sep" = "-09-01",
        "Oct" = "-10-01",
        "Nov" = "-11-01",
        "Dec" = "-12-01"
      )
    )
  ) %>%
  filter(!is.na(month))

# Create another tibble for the year column
columnYear <- columnMonth %>%
  filter(month == "-01-01") %>%
  mutate(year = seq(2010, 2021))

# Join the year and month tibbles to construct a date column
columnNameReplacement <- columnMonth %>%
  left_join(columnYear, by = c("column_name", "month")) %>%
  fill(year) %>%
  mutate(date = paste0(year, month)) %>%
  select(date, column_name)

columnFirstLast <- columnNameReplacement %>%
  filter(date %in% c(first(date), last(date))) %>%
  pull(date)

importColumnNamed <- importNoEmptyRows %>%
  rename(deframe(columnNameReplacement)) %>%
  slice(-c(1:2)) %>% # Remove rows containing year and month
  relocate(category_idn, category_eng)

importCategory <- importColumnNamed %>%
  mutate(
    across(
      cols = starts_with("category"),
      .fns = ~str_remove_all(.x, "[\\p{No}]") # Remove superscripts
    ),
    across(cols = starts_with("category"), .fns = ~str_trim(.x)),
    category_main = case_when(
      str_detect(category_eng, "^I++\\.|IV\\.|V\\.") ~ category_eng
    ),
    across(
      cols = starts_with("category"),
      .fns = ~str_remove_all(.x, "^I++\\.|IV\\.|V\\.")
    ),
    category_second = case_when(
      str_detect(category_eng, "^[A-D]\\.") ~ category_eng
    ),
    across(
      cols = starts_with("category"),
      .fns = ~str_remove_all(.x, "^[A-D]\\.")
    ),
    category_second = case_when(
      is.na(category_second) ~ category_main,
      TRUE ~ category_second
    ),
    across(cols = c(category_main, category_second), .fns = ~str_trim(.x))
  ) %>%
  fill(category_main, category_second) %>%
  relocate(category_main, category_second) %>%
  select(-category_idn) %>%
  rename("category_third" = "category_eng")

importClean <- importCategory %>%
  pivot_longer(
    cols = columnFirstLast[1]:columnFirstLast[2],
    names_to = "date",
    values_to = "import"
  )

categoryRawCapitalGoods <- c(
  "Raw materials and auxiliary goods",
  "Capital goods"
)

importGrouped <- importClean %>%
  filter(
    category_second %in% categoryRawCapitalGoods,
    # Remove totals by second category
    !(category_third %in% categoryRawCapitalGoods),
    !str_detect(category_third, "^o/w"), # Remove subcategories
    !str_detect(date, "annual")
  ) %>%
  mutate(
    date = ymd(date),
    import = as.double(import),
    category_second = str_remove_all(category_second, " and auxiliary goods"),
    # Make subcategories to simplify categories
    category_fourth = case_when(
      str_detect(category_third, "Food and beverages") ~ "Food and beverages",
      str_detect(category_third, "Industrial supplies") ~ "Industrial supplies",
      str_detect(category_third, "Parts and accessories") ~ "Parts and accessories",
      str_detect(category_third, "Fuels and lubricants") ~ "Fuels and lubricants",
      TRUE ~ category_third
    )
  ) %>%
  select(category_second, category_third, category_fourth, date, import)

# Calculate the sum of imports grouped by the fourth category
importCategoryFourth <- importGrouped %>%
  group_by(category_fourth, date) %>%
  mutate(import_total_category_fourth = sum(import)) %>%
  ungroup(category_fourth) %>%
  distinct(category_fourth, .keep_all = TRUE) %>%
  ungroup() %>%
  select(-c(import, category_third))

# Calculate the sum of imports grouped by the second category, which consists
# of raw materials and capital goods
importCategorySecond <- importCategoryFourth %>%
  group_by(category_second, date) %>%
  mutate(import_total_category_second = sum(import_total_category_fourth)) %>%
  ungroup()

# Calculate the contribution of each item in the fourth category to the
# year-on-year change of the second category
importContributionChange <- importCategorySecond %>%
  mutate(
    month = month(date),
    weight = import_total_category_fourth / import_total_category_second
  ) %>%
  group_by(category_fourth, month) %>%
  mutate(
    import_diff = import_total_category_fourth - lag(import_total_category_fourth),
    import_change_yoy = import_diff / lag(import_total_category_fourth) * 100,
    contribution_change_category_second = import_change_yoy * lag(weight)
  ) %>%
  ungroup()

importContributionChangeClean <- importContributionChange %>%
  mutate(year = year(date)) %>%
  filter(!is.na(contribution_change_category_second), year >= 2018) %>%
  select(
    c(
      category_second,
      category_fourth,
      date,
      import_total_category_fourth,
      import_change_yoy,
      contribution_change_category_second
    )
  )

importContributionChangeClean %>%
  write_csv(
    here(dirYear, dirProject, "result", "import-contribution-change-yoy.csv")
  )
