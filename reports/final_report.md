Final report
================
Sara Berg-Love
2020-03-15

  - [Introduction](#introduction)
  - [Female STEM tertiary graduates](#female-stem-tertiary-graduates)
  - [Potential correlations for low female STEM graduate
    percentages](#potential-correlations-for-low-female-stem-graduate-percentages)
      - [Perceived female respect](#perceived-female-respect)
      - [Government type](#government-type)
  - [Conclusion](#conclusion)
  - [References](#references)

``` r
# Libraries
library(tidyverse)
library(rnaturalearth)

# Parameters
  # File for female respect, female STEM percentages, and government types
file_respect_stem_governments <- 
  here::here("data/respect_stem_governments.rds")
  # File for female respect and female STEM graduate percentages
file_women_respect_stem_grad <- 
  here::here("data/women_respect_stem_grad.rds")

#===============================================================================

# Read in data for female respect and female STEM percentages
women_respect_stem_grad <- read_rds(file_women_respect_stem_grad)
# Read in data for female respect, female STEM percentage, and government types
respect_stem_governments <- read_rds(file_respect_stem_governments)
# Renaming countries for world map
country_rename <- 
  c(
    "Democratic Republic of the Congo" = "Congo (Kinshasa)",
    "Republic of Congo" = "Congo Brazzaville",
    "Republic of Serbia" = "Serbia",
    "The Bahamas" = "Bahamas",
    "United Republic of Tanzania" = "Tanzania"
  )
```

## Introduction

Worldwide, there is a problem with the number of women in Science,
Technology, Engineering, and Math (STEM) fields. These fields are
significantly dominated by men, which has a detrimental impact on
progress. Studies have shown that gender diversity in teams generate
more ideas that lead to important innovations \[1\]. This is one of the
United Nations Educational, Scientific, and Cultural Organization’s
(UNESCO) priority areas, improving gender equality in STEM fields \[2\].
Although it is not one of the United Nations Development Program’s
(UNDP) Sustainable Development Goals (SDG), it can be looked at through
the lens of SDG 4 which is quality education inclusive of gender at all
levels \[3\].

## Female STEM tertiary graduates

The following data is from the UNESCO Institute for Statistics\[3\].

``` r
women_respect_stem_grad %>% 
  filter(year == 2017) %>% 
  group_by(country) %>% 
  summarize(perct_women_stem_grad_avg = median(perct_women_stem_grad)) %>% 
  ggplot(aes(perct_women_stem_grad_avg)) +
  geom_histogram(binwidth = 2, na.rm = TRUE) +
  geom_vline(
    aes(xintercept = median(perct_women_stem_grad_avg, na.rm = TRUE)), 
    color = "blue3"
  ) +
  scale_x_continuous(
    breaks = scales::breaks_width(5), 
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  labs(
    title = 
      "Worldwide the average female STEM tertiary graduates was only 12.5% in 2017",
    x = "Percentage of female STEM graduates",
    y = "Number of data points",
    caption = "Source: UNESCO Institute for Statistics"
  )
```

<img src="final_report_files/figure-gfm/avg_stem-1.png" width="100%" />

This shows that unfortunately for the last complete year of data in
2017, the worldwide average percentage of female tertiary graduates in
STEM fields was only 12.5%.

``` r
women_respect_stem_grad %>% 
  filter(year %in% c(1999:2018)) %>% 
  ggplot(aes(year, perct_women_stem_grad)) +
  geom_hline(
    data = . %>% filter(year == 1999), 
    aes(yintercept = median(perct_women_stem_grad)), 
    color = "blue2"
  ) +
  geom_boxplot(aes(group = year), varwidth = TRUE, na.rm = TRUE) +
  ggrepel::geom_text_repel(
    data = . %>% filter(perct_women_stem_grad > 40), 
    aes(label = country), 
    size = 3
  ) +
  scale_x_continuous(breaks = scales::breaks_width(2)) +
  annotate(
    "text",
    x = 1998,
    y = 12.75,
    label = "1999\naverage",
    color = "blue2",
    size = 2.5
  ) +
  labs(
    title = "Median female STEM graduates has not increased over time",
    subtitle = "Although there are increasing numbers of high outliers",
    x = NULL,
    y = "Percentage of female tertiary graduates in STEM fields",
    caption = "Source: UNESCO Institute for Statistics"
  )
```

<img src="final_report_files/figure-gfm/stem_time-1.png" width="100%" />

This low percentage is not only found in 2017 though. What is even worse
is that these percentages have been constantly low since 1999, with a
few exceptions such as Liberia, Liechtenstein, Gambia, and Oman. But
even these countries have not had consistently higher percentages of
female STEM graduates.

Top 5 countries in 1998 for percentage of female tertiary STEM
graduates:

``` r
women_respect_stem_grad %>% 
  arrange(year) %>% 
  filter(year == first(year) + 2) %>% 
  top_n(5, perct_women_stem_grad) %>% 
  arrange(desc(perct_women_stem_grad)) %>% 
  select(country, year, perct_women_stem_grad) %>% 
  knitr::kable()
```

| country     | year | perct\_women\_stem\_grad |
| :---------- | ---: | -----------------------: |
| Liberia     | 2000 |                 40.22140 |
| South Korea | 2000 |                 26.53618 |
| Ireland     | 2000 |                 23.74962 |
| Turkey      | 2000 |                 23.14826 |
| Qatar       | 2000 |                 21.30653 |

Top 5 countries in 2018 for percentage of female tertiary STEM
graduates:

``` r
women_respect_stem_grad %>% 
  arrange(year) %>% 
  filter(year == last(year) - 1) %>% 
  top_n(5, perct_women_stem_grad) %>% 
  arrange(desc(perct_women_stem_grad)) %>% 
  select(country, year, perct_women_stem_grad) %>% 
  knitr::kable()
```

| country           | year | perct\_women\_stem\_grad |
| :---------------- | ---: | -----------------------: |
| Oman              | 2018 |                 41.00775 |
| Tunisia           | 2018 |                 36.46269 |
| Brunei Darussalam | 2018 |                 33.74094 |
| Myanmar           | 2018 |                 31.00917 |
| Algeria           | 2018 |                 30.89274 |

Looking at the top 5 countries for both 2000 and 2018 in terms of
percentage of female STEM tertiary graduates, it shows that none of the
same countries are consistently good at supporting women in these
fields. However, the average percent of increase, or decrease, per year
is also worth looking at.

``` r
women_respect_stem_grad %>% 
  arrange(year) %>%
  group_by(country) %>%
  summarize(
    perct_change_per_year = 
      (last(perct_women_stem_grad) - first(perct_women_stem_grad)) / 
      (last(year) - first(year))
  ) %>% 
  top_n(5, wt = perct_change_per_year) %>% 
  arrange(desc(perct_change_per_year)) %>% 
  knitr::kable()
```

| country           | perct\_change\_per\_year |
| :---------------- | -----------------------: |
| Gambia            |                 5.901910 |
| Cayman Islands    |                 3.170290 |
| Oman              |                 1.859776 |
| Cabo Verde        |                 1.810840 |
| Brunei Darussalam |                 1.378178 |

From this, it shows that Gambia and the Cayman Islands are increasing
the most over time, but neither of them are in the top 5. Oman, as the
3rd highest percentage increase is top in 2018, which shows a good
positive trend, especially for a more conservative country.

Even though this data is worldwide, it is worth pulling out the STEM
female percentage data for the United States specifically.

``` r
  women_respect_stem_grad %>% 
    filter(country == "United States of America") %>% 
    ggplot(aes(year, perct_women_stem_grad)) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) +
    scale_x_continuous(breaks = scales::breaks_width(width = 1)) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 0.1, scale = 1)
    ) +
    labs(
      title = 
          "Percentage of female tertiary graduates in STEM fields\nin the United States of America",
      x = "Year",
      y = "Percentage of total tertiary graduates in STEM fields",
      caption = "Source: UNESCO Institute for Statistics"
    )
```

<img src="final_report_files/figure-gfm/stem_usa-1.png" width="100%" />

From this data, we can see that the US has been on a positive
trajectory, increasing its female STEM graduate percentage, since 2013.
It’s still not very high, and about 2% below the worldwide average, but
at least it is increasing. This could be due to increasing resources
being devoted to promoting girls in STEM starting in elementary school.

## Potential correlations for low female STEM graduate percentages

With these percentages of women in STEM fields so low, it is worth
seeing if there is a correlation with another factor. That could allow
decision-makers worldwide to learn from successful countries and support
women more in their own countries.

### Perceived female respect

One possible correlation could be due to if women are respected in that
country. If they feel they are respected, they could be more likely to
pursue a career they are interested in, regardless of its gender
diversity.

The Gallup World Poll started in 2005 and surveys about 1,000 people per
year in more than 160 countries worldwide \[4\]. The interview questions
are about global issues as well as region-specific issues. The global
questions are asked in the same way in order to provide comparisons
across countries. One question that was asked consistently in many
countries over 8 years was “Do you believe that women in this country
are treated with respect and dignity, or not?”\[5\]. The following chart
visualizes that data broken down by region.

``` r
women_respect_stem_grad %>% 
  filter(prop_type == "All") %>% 
  group_by(region, year) %>% 
  summarize(prop = mean(prop)) %>% 
  ungroup() %>% 
  ggplot(aes(year, prop, color = region)) +
  geom_line() +
  ggrepel::geom_text_repel(
    data = . %>% filter(year == 2017), 
    aes(label = region), 
    size = 2, 
    nudge_x = 1, 
    point.padding = 0.25
  ) +
  scale_x_continuous(breaks = seq(2009, 2017, 2), limits = c(2009, 2018)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "none") +
  labs(
    title = "Are women respected in your country?", 
    x = NULL, 
    y = 'Percentage responding "Yes"',
    color = "Gender",
    caption = "Source: Gallup World Poll"
  )
```

    ## `summarise()` has grouped output by 'region'. You can override using the `.groups` argument.

<img src="final_report_files/figure-gfm/respect_by_region-1.png" width="100%" />

In general, perceived female respect varies greatly by region. The Latin
America and Caribbean has the lowest respect percentages by far, around
35% in 2017, with Sub-Saharan being a little less than double at 65% in
2017. Southeast Asia and the Australia-New Zealand regions have the
highest percentages of female respect, around 85%. Overall, female
respect worldwide has been declining since at least 2015.

#### Perceived female respect by gender

It would make sense that men and women have different perspectives on
whether or not women in their country are respected. So, it is
worthwhile breaking down the Gallup World Poll data by gender.

``` r
women_respect_stem_grad %>% 
  filter(prop_type != "All") %>% 
  ggplot(aes(factor(1), prop, color = prop_type)) +
  geom_boxplot(varwidth = TRUE) +
  facet_grid(cols = vars(year)) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("Female" = "deep pink", "Male" = "blue")) +
  theme(legend.position = "bottom") +
  labs(
    title = "Men believe women are more respected than women do",
    x = NULL,
    y = "Percentage believing women are respected",
    color = NULL,
    caption = "Source: Gallup World Poll"
  )
```

<img src="final_report_files/figure-gfm/respect_gender-1.png" width="100%" />

There is not a huge change from 2009 to 2017 in terms of average respect
for women, being consistently between 60-75%, but men consistently
believed women were more respected than women believed they were.

Looking at this data broken down by gender and region, some more trends
emerge:

``` r
women_respect_stem_grad %>% 
  drop_na(prop_type) %>% 
  group_by(region, year, prop_type) %>% 
  mutate(prop = mean(prop)) %>% 
  ungroup() %>% 
  ggplot(aes(year, prop, color = prop_type)) +
  geom_line() +
  facet_wrap(vars(region), ncol = 3) +
  scale_x_continuous(breaks = scales::breaks_width(2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(
    values = c("All" = "black", "Female" = "deep pink", "Male" = "blue")
    ) +
  theme(legend.position = "bottom") +
  labs(
    title = "Are women respected in your country?", 
    x = NULL, 
    y = 'Percentage responding "Yes"',
    color = "Gender",
    caption = "Source: Gallup World Poll"
  )
```

<img src="final_report_files/figure-gfm/respect_region_plot_large-1.png" width="100%" />

Opinions across genders are most clearly aligned in Southeast Asia,
where they are clearly stratified with perceptions of female respect
being much lower for women than men. Overall female respect is
significantly by far consistently the lowest in Latin America and the
Caribbean. Southeast Asia and Australia/New Zealand have the highest
consistent percentages of female respect from 2009 to 2017.

Again, it is worthwhile pulling the United States data out specifically:

``` r
  women_respect_stem_grad %>% 
    filter(country == "United States of America") %>% 
    drop_na(prop_type) %>% 
    ggplot(aes(year, prop, color = prop_type)) +
    geom_point(na.rm = TRUE) +
    geom_line(na.rm = TRUE) +
    scale_x_continuous(breaks = scales::breaks_width(2)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(
      values = c("All" = "black", "Female" = "deep pink", "Male" = "blue")
    ) +
    theme(legend.position = "bottom") +
    labs(
      title = 
        "Women in the United States felt significantly less respected in 2016", 
      x = "Year", 
      y = "Percentage of perceived female respect",
      color = "Gender",
      caption = "Source: Gallup World Poll"
    )
```

<img src="final_report_files/figure-gfm/respect_country-1.png" width="100%" />

Pulling out the US specifically, we see that there was a decrease in
2016, especially for women. This could be due to the sexist comments by
Donald Trump. Men actually thought that females were respected more
then, potentially because Hillary Clinton was running for president. The
increase in women’s perceptions for 2017 could be due to the \#MeToo
movement as they felt their voices were getting heard, while men were
made more aware of the issue and realized women were less respected than
they thought.

#### Perceived female respect and female STEM graduates

``` r
women_respect_stem_grad %>% 
  filter(year %in% c(2009, 2011, 2013, 2015, 2017), prop_type == "All") %>% 
  ggplot(aes(prop, perct_women_stem_grad, color = factor(year))) +
  geom_point(aes(alpha = 0.5), show.legend = FALSE, na.rm = TRUE) +
  geom_smooth(se = FALSE, na.rm = TRUE) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1), 
    breaks = scales::breaks_width(0.1)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  coord_fixed(ratio = 0.01) +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(
    title = 
      "No real correlation between higher female respect and STEM graduates",
    x = 'Percentage believing women are respected in their country',
    y = "Percentage of female tertiary graduates in STEM fields",
    color = "Year",
    caption = "Sources: Gallup World Poll, UNESCO Institute for Statistics"
  )
```

<img src="final_report_files/figure-gfm/respect_STEM_basic_correlation-1.png" width="100%" />

Looking at overall trends for 5 years between 2009 and 2017, there is
not an overall correlation, as this graph shows. Breaking it down into
male and female perspectives of perceived female respect, there still is
not an overall trend between if people think women are more respected in
their country and the percentage of female STEM graduates.

Since there is not an overall worldwide trend, it is worth looking at
the data broken down into world regions.

``` r
women_respect_stem_grad %>% 
  filter(prop_type == "All") %>% 
  drop_na(region) %>% 
  group_by(region, year) %>% 
  summarize(
    perct_women_stem_grad = mean(perct_women_stem_grad, na.rm = TRUE), 
    prop = mean(prop, na.rm = TRUE)
  ) %>% 
  drop_na(prop, perct_women_stem_grad) %>% 
  ggplot(aes(prop, perct_women_stem_grad, color = region)) +
  geom_path(aes(alpha = year), show.legend = FALSE) +
  geom_point(
    data = . %>% filter(year == max(year)), 
    show.legend = FALSE
  ) +
  ggrepel::geom_text_repel(
    aes(label = region), 
    data = . %>% filter(year == max(year)), 
    nudge_y = 0.01, 
    hjust = 0,
    show.legend = FALSE, 
    segment.color = "gray50",
    size = 2.5
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1), 
    breaks = scales::breaks_width(0.05)) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
  ) +
  theme(legend.position = "bottom") +
  labs(
    title = "Are women respected in your country?", 
    subtitle = "Data from 2009 - 2017",
    x = 'Percentage of perceived female respect',
    y = "Percentage of female tertiary graduates in STEM fields",
    caption = "Sources: Gallup World Poll, UNESCO Institute for Statistics"
  )
```

    ## `summarise()` has grouped output by 'region'. You can override using the `.groups` argument.

<img src="final_report_files/figure-gfm/respect_STEM_regions-1.png" width="100%" />

For the aggregate data, the Middle East and North Africa region is one
outlier, with surprisingly high female respect for what seems like to
the rest of the world a very conservative culture for women. However, it
seems that the respect is even improving. They also have relatively high
female tertiary graduate in STEM fields presentages. On the other end,
Latin America and the Caribbean have both the lowest respect and
percentage of female STEM graduates. Both Southeast Asia and South Asia
have decreased their female STEM graduate percentages significantly
since 2009, but female respect has mostly increased. While female STEM
graduate percentages there seem to be increasing, respect of women is
unfortunately decreasing overall.

There is not really a difference on female tertiary STEM graduates when
broken down into female and male perceived female respect regionally.
Females still consistently think female respect percentages are lower
though.

``` r
plot_regional_countries <- function(single_region) {
  women_respect_stem_grad %>% 
    filter(region == single_region, prop_type == "All") %>% 
    drop_na(prop, perct_women_stem_grad) %>% 
    group_by(country, year) %>% 
    summarize(
      perct_women_stem_grad = mean(perct_women_stem_grad), 
      prop = mean(prop)
    ) %>% 
    arrange(country, year) %>% 
    ggplot(aes(prop, perct_women_stem_grad, color = country)) +
    geom_path(aes(alpha = year), show.legend = FALSE) +
    geom_point(aes(alpha = year), show.legend = FALSE) +
    ggrepel::geom_text_repel(
      aes(label = country), 
      data = . %>% filter(year == max(year)), 
      nudge_x = -0.01, 
      hjust = 0,
      size = 3,
      show.legend = FALSE, 
      segment.color = "gray50"
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1, scale = 1)
    ) +
    theme(legend.position = "bottom") +
    labs(
      title = str_c("Are women respected in your country? - ", single_region), 
      subtitle = "Data from 2009 - 2017",
      x = 'Percentage responding "Yes"',
      y = "Percentage of female tertiary graduates in STEM fields",
      caption = "Sources: Gallup World Poll, UNESCO Institute for Statistics"
    ) 
}
```

``` r
plot_regional_countries("Middle East and North Africa")
```

    ## `summarise()` has grouped output by 'country'. You can override using the `.groups` argument.

<img src="final_report_files/figure-gfm/respect_STEM_regional_countries-1.png" width="100%" />

``` r
plot_regional_countries("Latin America and the Caribbean")
```

    ## `summarise()` has grouped output by 'country'. You can override using the `.groups` argument.

<img src="final_report_files/figure-gfm/respect_STEM_regional_countries-2.png" width="100%" />

For the Middle East and North Africa region, Tunisia and Turkey are both
outliers. Tunisia potentially has the highest female STEM graduate
percentage due to its pro-democracy shift in the Arab Spring leading to
political and economic reforms. Turkey might have the lowest female
respect and a fairly low percentage of female STEM graduates because of
the role women are expected to play in society \[3\]. Only 34% of women
work outside the home in Turkey and are expected to focus solely on
being homemakers.

For the Latin America and Caribbean region, Peru is the main outlier. It
has the highest female STEM graduate percentage, but female respect has
been decreasing to less than 25%. The increasing STEM graduate
percentage could be correlated with its increasing GDP and decreasing
overall poverty, which is one of the highest in the region \[4\].

Looking specifically at the United States again:

``` r
women_respect_stem_grad %>% 
  filter(country == "United States of America", prop_type == "All") %>% 
  arrange(country, year) %>% 
  drop_na(prop, perct_women_stem_grad) %>% 
  ggplot(aes(prop, perct_women_stem_grad)) +
  geom_point() +
  geom_path(aes(alpha = year), show.legend = FALSE) +
    ggrepel::geom_text_repel(
    aes(label = year), 
    nudge_y = -0.02, 
    size = 3,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1), 
    breaks = scales::breaks_width(0.02)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    breaks = scales::breaks_width(2),
    limits = c(1, 12)
  ) +
  coord_fixed(ratio = 0.01) +
  theme(legend.position = "bottom") +
    labs(
    title = 
      "Respect for women in the US is decreasing\nbut percentage of female STEM graduates is increasing",
    x = 'Percentage of perceived female respect',
    y = "Percentage of female tertiary graduates in STEM fields",
    caption = "Sources: Gallup World Poll, UNESCO Institute for Statistics"
  )
```

<img src="final_report_files/figure-gfm/respect_STEM_select_countries-1.png" width="100%" />

The percentage of female STEM tertiary graduates is slowly increasing in
the United States, but the percentage of female respect has decreased
since 2012. This is not a good trend, especially as across the country
there have been increasing amounts of financial support and programs for
girls in STEM and this only shows a small increase.

So overall there is a general positive trend of respect and STEM grads
in some regions worldwide, but nothing too conclusive.

### Government type

Since there was not really a correlation between female respect and
female STEM graduates, it is worth seeing if another variable, such as
the type of government a country has, effects the percentage of female
STEM field graduates.

The following data is a combination of the data previously analyzed from
the UNESCO Institute of Statistics on female tertiary STEM graduates and
the Gallup World Poll on female respect, with the addition of data from
Wikipedia on the Democracy Index and government types of countries
around the world \[3\]\[5\]\[8\]. The Democracy Index is created by the
Economist Intelligence Unit and measures the range of democracy in 166
countries. The scale is from 0-10, broken down like this:

  - 0-4: Authoritarian government
  - 4-6: Hybrid regime government
  - 6-8: Flawed democracy
  - 8-10: Full democracy

<!-- end list -->

``` r
respect_stem_governments %>% 
  drop_na(regime) %>% 
  mutate(
    regime = 
      factor(
        regime, 
        levels = 
          c(
            "Authoritarian", 
            "Flawed democracy", 
            "Hybrid regime", 
            "Full democracy"
          )
      )
  ) %>% 
  distinct(country, regime) %>% 
  ggplot((aes(regime))) +
  geom_bar(na.rm = TRUE) +
  labs(
    title = "Authoritarian and flawed democracies are the most common regimes",
    x = "Government type",
    y = "Number of countries",
    caption = "Source: Wikipedia - Democracy Index"
  )
```

<img src="final_report_files/figure-gfm/govt_type-1.png" width="100%" />

Authoritarian and flawed democracies are the most common government
types, followed by hybrid regimes and full democracies.

``` r
respect_stem_governments %>%
  filter(prop_type == "All", year %in% seq(2009, 2017, 2)) %>% 
  drop_na(regime) %>% 
  ggplot(aes(x = factor(1), y = prop)) +
  geom_boxplot(aes(color = regime), varwidth = TRUE) +
  facet_grid(cols = vars(year)) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "bottom") +
  labs(
    title = 
      "Authoritarian and full democracy governments have the\nhighest female respect percentages",
    x = NULL,
    y = "Percent of perceived female respect",
    color = "Government type",
    caption = 
      "Sources: Gallup World Poll, UNESCO Institute for Statistics, Wikipedia - Democracy Index"
  )
```

![](final_report_files/figure-gfm/respect_stem_democracy-1.png)<!-- -->

``` r
respect_stem_governments %>%
  filter(prop_type == "All", year %in% seq(2009, 2017, 2)) %>% 
  drop_na(regime, perct_women_stem_grad) %>% 
  ggplot(aes(x = factor(1), y = perct_women_stem_grad)) +
  geom_boxplot(aes(color = regime), varwidth = TRUE) +
  facet_grid(cols = vars(year)) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
  theme(legend.position = "bottom") +
  labs(
    title = 
      "Authoritarian governments tend to have\nhigher female STEM graduate percentages",
    x = NULL,
    y = "Percent of female STEM tertiary graduates",
    color = "Government type",
    caption = 
      "Sources: Gallup World Poll, UNESCO Institute for Statistics, Wikipedia - Democracy Index")
```

![](final_report_files/figure-gfm/respect_stem_democracy-2.png)<!-- -->

Government type seems to have a greater effect on percentage of
perceived female respect. Authoritarian governments and full democracies
tend to have the highest female respect percentages, followed by flawed
democracies, then hybrid regimes. Overall, the median percentages of
respect have stayed constant for authoritarian regimes, slightly
decreased for full democracies, increased for flawed democracies, and
cycled up and down for hybrid regimes. The type of government has less
of an impact on percentages of women in STEM fields, with authoritarian
governments having slightly higher percentages more recently, and the
rest of the governments being about the same.

``` r
respect_stem_governments %>%
  filter(prop_type == "All", year == 2017) %>% 
  drop_na(regime) %>% 
  ggplot((aes(prop, perct_women_stem_grad, color = regime))) +
  geom_point(aes(alpha = 0.5), na.rm = TRUE, show.legend = FALSE) +
  geom_smooth(na.rm = TRUE, se = FALSE) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
  theme(legend.position = "bottom") +
  labs(
    title = 
      "Neigher government type nor percentage of female respect\naffect female STEM graduates in a country",
    subtitle = "2017 data",
    x = "Percent of perceived female respect",
    y = "Percent of female STEM tertiary graduates",
    color = "Government type",
    caption = 
      "Sources: Gallup World Poll, UNESCO Institute for Statistics, Wikipedia - Democracy Index"
    )
```

<img src="final_report_files/figure-gfm/respect_stem_govt-1.png" width="100%" />

This graph shows there is not a correlation between democracy, or even
type of democracy, and proportion of female STEM tertiary graduates or
perceived female respect.

## Conclusion

``` r
world_map <- 
  ne_countries(returnclass = 'sf') %>% 
  filter(continent != "Antarctica") %>% 
  select(country = sovereignt, geometry) %>% 
  mutate(country = recode(country, !!! country_rename, .default = country))

latest_stem_data <- 
  women_respect_stem_grad %>% 
  select(country, year, perct_women_stem_grad) %>% 
  drop_na(perct_women_stem_grad) %>% 
  arrange(country, year) %>% 
  group_by(country) %>% 
  filter(year == last(year)) %>% 
  ungroup() %>% 
  distinct(country, perct_women_stem_grad, year)

world_map %>% 
  left_join(latest_stem_data, by = "country") %>% 
  ggplot() +
  geom_sf(data = world_map) +
  geom_sf(aes(fill = perct_women_stem_grad)) +
  scale_fill_distiller(
    limits = c(0, 40),
    labels = c("0%", "10%", "20%", "30%", "40+%"),
    palette = "Greens",
    direction = 1,
    na.value = "gray90"
  ) +
  guides(
    fill = guide_colorbar(barheight = 0.5, barwidth = 15, title = NULL)
  ) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(
    title = "Percent of female STEM tertiary graduates worldwide",
    subtitle = "Most recent data available for each country",
    caption = "Sources: UNESCO Institute for Statistics, Natural Earth"
  )
```

<img src="final_report_files/figure-gfm/map-1.png" width="100%" />

As this world map shows, the percentage of female STEM tertiary
graduates varies greatly, even within regions. Also, most countries have
less than 20% of female STEM graduates, which is not a good balance.

Overall, there does not appear to be much of a correlation between
increasing female respect in a country, from a male or female
perspective, leading to an increased percentage of female STEM tertiary
graduates. Also, there does not appear to be a correlation between
different types of governments leading to increased percentages of
female STEM tertiary graduates. However, there are still certain
countries that are doing better with respecting women and supporting
them going into STEM fields and some that have downward trends as well.
This could be useful in the future in knowing where to look for policies
that do or do not work in supporting women.

## References

\[1\] Cristina Díaz-García, Angela González-Moreno & Francisco Jose
Sáez-Martínez. 2013. [Gender diversity within R\&D teams: Its impact on
radicalness of
innovation.](https://www.tandfonline.com/doi/abs/10.5172/impp.2013.15.2.149)
Innovation, 15:2, 149-160, DOI: 10.5172/impp.2013.15.2.149.

\[2\] UNESCO. [STEM and Gender Advancement
(SAGA).](http://www.unesco.org/new/en/natural-sciences/priority-areas/gender-and-science/improving-measurement-of-gender-equality-in-stem/stem-and-gender-advancement-saga/)

\[3\] [UNESCO Institute for Statistics.](http://data.uis.unesco.org/)

\[4\] [Methodology: How does the Gallup World Poll
Work?](https://www.gallup.com/178667/gallup-world-poll-work.aspx)

\[5\] Gallup World Poll. [“Do you believe that women in this country are
treated with respect and dignity, or
not?”](https://wpr.gallup.com/home.aspx?ref=logo)

\[6\] Lowen, Mark. BBC News. March 1, 2018. [Women challenge Turkey
traditions for right to
work.](https://www.bbc.com/news/world-europe-43197642)

\[7\] The World Bank. October 1, 2011. [Data by country:
Peru.](https://data.worldbank.org/country/peru)

\[8\] Wikipedia. [“Democracy
Index”.](https://en.wikipedia.org/wiki/Democracy_Index)
