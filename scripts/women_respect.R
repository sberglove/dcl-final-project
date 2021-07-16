# Reads in and writes out data on various questions on the theme of if people
# believe that women in that country are treated with respect.

# Source: Gallup. (2009 - 2019). "Do you believe that women in this country are
#         treated with respect and dignity, or not?" Retrieved from
#         https://wpr.gallup.com/question.aspx?R=&C=&CA=42&SU=43&T=&Q=&S=&E=&Y=true&P=1&I=10096&RI=0&CI=0&CAI=9&SCI=1.

# Author: Sara Berg-Love
# Version: 2020-03-05

# Libraries
library(tidyverse)

# Parameters
file_raw <- here::here("data-raw/gallup_respect_gender_region_plus.txt")

var_cols <-
  cols(
    WP5 = col_character(),
    wgt = col_double(),
    FIELD_DATE = col_character(),
    REG_GLOBAL = col_character(),
    REG2_GLOBAL = col_character(),
    WP10416 = col_character(),
    WP10418 = col_character(),
    WP10507 = col_character(),
    WP10588 = col_character(),
    WP11265 = col_character(),
    WP1140 = col_character(),
    WP1145 = col_character(),
    WP1219 = col_character(),
    WP1220 = col_double(),
    WP1223 = col_character(),
    WP12344 = col_character(),
    wp12714 = col_character(),
    wp12715 = col_character(),
    WP1342 = col_character(),
    WP1348 = col_character(),
    WP1349 = col_character(),
    WP16025 = col_character(),
    wp1697 = col_character(),
    WP5408 = col_character(),
    WP5417 = col_character(),
    WP9050 = col_character(),
    YEAR_CALENDAR = col_double()
  )

var_rename <-
  c(
    "country" = "WP5",
    "region" = "REG_GLOBAL",
    "region_2" = "REG2_GLOBAL",
    "weight" = "wgt",
    "gender" = "WP1219",
    "women_respected_1" = "WP9050",
    "year" = "YEAR_CALENDAR",
    "women_respected_2" = "WP10416",
    "equal_education_1" = "WP10418",
    "equal_education_2" = "WP10507",
    "men_rights_scare_job" = "WP10588",
    "internet_access" = "WP11265",
    "women_men_same_rights" = "WP1140",
    "women_any_job" = "WP1145",
    "age" = "WP1220",
    "marital_status" = "WP1223",
    "work_gender_equality" = "WP12344",
    "students_prep_academically" = "wp12714",
    "college_essential_success" = "wp12715",
    "woman_child_outside_marriage" = "WP1342",
    "female_career_marriage_delay" = "WP1348",
    "married_woman_career_over_family" = "WP1349",
    "satisfaction_educ_opp" = "WP16025",
    "satisfaction_own_educ" = "wp1697",
    "education_country" = "WP5408",
    "women_men_equal_opp" = "WP5417"
  )

file_out <- here::here("data/women_respect.rds")

#===============================================================================

file_raw %>%
  read_csv(col_types = var_cols) %>%
  rename(!!! var_rename) %>%
  select(-FIELD_DATE) %>%
  write_rds(file_out)
