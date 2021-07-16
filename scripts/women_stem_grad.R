# Reads in and writes out data on the percentage of female tertiary graduates
# in science, technology, and engineering fields in different countries.

# Source: UNESCO Institute for Statistics. (1996 - 2018). Education >
#         Completion > Distribution of tertiary graduates >
#         Distribution of tertiary graduates by field of study.
#         Retrieved from http://data.uis.unesco.org/.

# Author: Sara Berg-Love
# Version: 2020-02-15

# Libraries
library(tidyverse)

# Parameters
file_raw <- here::here("data-raw/unesco_stem_grad_perct_women.csv")

var_cols <-
  cols(
    Country = col_character(),
    Time = col_integer(),
    Value = col_double()
  )

var_rename <-
  c(
    "country" = "Country",
    "year" = "Time",
    "perct_women_stem_grad" = "Value"
  )

var_select <-
  c(
    "country",
    "year",
    "perct_women_stem_grad"
  )

file_out <- here::here("data/women_stem_grad.rds")

#===============================================================================

file_raw %>%
  read_csv(col_types = var_cols) %>%
  rename(!!! var_rename) %>%
  drop_na(perct_women_stem_grad) %>%
  select(!!! var_select) %>%
  write_rds(file_out)
