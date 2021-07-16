# Reads in and writes out data on combining if people believe that women in
# that country are treated with respect and the percentage of female tertiary
# graduates in STEM fields in that country.

# Author: Sara Berg-Love
# Version: 2020-03-14

# Libraries
library(tidyverse)

# Parameters
file_respect <- here::here("data/women_respect.rds")

file_stem_grads <- here::here("data/women_stem_grad.rds")

file_out <- here::here("data/women_respect_stem_grad.rds")

countries_recode <-
  c(
    "United States" = "United States of America",
    "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
    "Iran (Islamic Republic of)" = "Iran",
    "China, Hong Kong Special Administrative Region" = "Hong Kong",
    "Lao People's Democratic Republic" = "Laos",
    "Republic of Moldova" = "Moldova",
    "Syrian Arab Republic" = "Syria",
    "Viet Nam" = "Vietnam",
    "Democratic Republic of the Congo" = "Congo (Kinshasa)",
    "Congo" = "Congo Brazzaville",
    "Republic of Korea" = "South Korea"
  )

#===============================================================================

women_respect <-
  file_respect %>%
  read_rds() %>%
  mutate_at(
    vars(country),
    ~ recode(., !!! countries_recode)
  )

women_respect_prop_gender <-
  women_respect %>%
  drop_na(women_respected_1, gender) %>%
  count(
    country,
    year,
    region,
    region_2,
    gender,
    women_respected_1,
    wt = weight,
    name = "prop_respect"
  ) %>%
  group_by(country, year, gender) %>%
  mutate(prop_respect = prop_respect / sum(prop_respect)) %>%
  ungroup() %>%
  filter(women_respected_1 == "Yes") %>%
  pivot_wider(names_from = gender, values_from = prop_respect) %>%
  select(-women_respected_1)

women_respect_props <-
  women_respect %>%
  drop_na(women_respected_1) %>%
  count(
    country,
    year,
    region,
    region_2,
    women_respected_1,
    wt = weight,
    name = "prop_respect"
  ) %>%
  group_by(country, year) %>%
  mutate(All = prop_respect / sum(prop_respect)) %>%
  ungroup() %>%
  filter(women_respected_1 == "Yes") %>%
  left_join(
    women_respect_prop_gender, by = c("country", "year", "region", "region_2")
  ) %>%
  pivot_longer(
    cols = c(All, Female, Male),
    names_to = "prop_type", values_to = "prop"
  ) %>%
  select(-women_respected_1, -prop_respect)

file_stem_grads %>%
  read_rds() %>%
  mutate_at(
    vars(country),
    ~ recode(., !!! countries_recode)
  ) %>%
  full_join(women_respect_props, by = c("country", "year")) %>%
  write_rds(file_out)
