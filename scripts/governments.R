# Scrapes data from Wikipedia about the democracy index of countries around the
# world.

# Source: Wikipedia. "Democracy Index." Retrieved from
#         https://en.wikipedia.org/wiki/Democracy_Index.

# Author: Sara Berg-Love
# Version: 2020-03-14

# Libraries
library(tidyverse)
library(rvest)

# Parameters
  # File name to write to
file_out <- here::here("data/governments.rds")
  # URL of data to scrape
data_url <- "https://en.wikipedia.org/wiki/Democracy_Index"
  # HTML location of data table
css_selector <- "#mw-content-text > div > table:nth-child(22)"

countries_recode <-
  c(
    "United States" = "United States of America",
    "Democratic Republic of the Congo" = "Congo (Kinshasa)",
    "Congo" = "Congo Brazzaville",
    "South Korea[n 1]" = "South Korea"
  )

regime_levels <-
  c(
    "Authoritarian", "Hybrid regime", "Flawed democracy", "Full democracy"
  )

#===============================================================================

governments <-
  data_url %>%
  read_html() %>%
  html_node(css = css_selector) %>%
  html_table() %>%
  as_tibble() %>%
  filter(Rank != "Rank") %>%
  mutate(
    country = recode(Country, !!! countries_recode, .default = Country),
    score = as.double(Score),
    regime = factor(Regimetype, regime_levels)
  ) %>%
  select(rank = Rank, country, score, regime) %>%
  write_rds(file_out)
