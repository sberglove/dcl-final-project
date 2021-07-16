# Reads in and writes out data on combining if people believe that women in
# that country are treated with respect, the percentage of female tertiary
# graduates in STEM fields in that country, and the democracy index and type of
# that country.

# Author: Sara Berg-Love
# Version: 2020-03-14

# Libraries
library(tidyverse)

# Parameters
  # File name to write to
file_goverments <- here::here("data/governments.rds")

file_respect_stem <- here::here("data/women_respect_stem_grad.rds")

file_out <- here::here("data/respect_stem_governments.rds")

#===============================================================================

respect_stem_governments <-
  file_respect_stem %>%
  read_rds() %>%
  full_join(file_goverments %>% read_rds(), by = "country") %>%
  write_rds(file_out)
