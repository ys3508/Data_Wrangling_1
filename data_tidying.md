Data Tidying
================

# load package

``` r
library(tidyverse) # tidyverse is a range of packages
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
options(tibble.print_min = 5)
```

# pivot_longer

In data import, we used the haven package to load the PULSE biomarkers
dataset from a .sas7bdat. Let’s reload those data and take a closer
look:

``` r
pulse_df = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") %>%
  janitor::clean_names()

pulse_df
```

    ## # A tibble: 1,087 × 7
    ##      id   age sex   bdi_score_bl bdi_score_01m bdi_score_06m bdi_score_12m
    ##   <dbl> <dbl> <chr>        <dbl>         <dbl>         <dbl>         <dbl>
    ## 1 10003  48.0 male             7             1             2             0
    ## 2 10015  72.5 male             6            NA            NA            NA
    ## 3 10022  58.5 male            14             3             8            NA
    ## 4 10026  72.7 male            20             6            18            16
    ## 5 10035  60.4 male             4             0             1             2
    ## # … with 1,082 more rows

With our new understanding of tidy data, we quickly recognize a problem:
the BDI score is spread across four columns, which correspond to four
observation times. We can fix this problem using pivot_longer:

``` r
pulse_tidy_data = 
  pivot_longer(
    pulse_df, #dataset
    bdi_score_bl:bdi_score_12m, #column range
    names_to = "visit", #new column name
    values_to = "bdi") # value

pulse_tidy_data
```

    ## # A tibble: 4,348 × 5
    ##      id   age sex   visit           bdi
    ##   <dbl> <dbl> <chr> <chr>         <dbl>
    ## 1 10003  48.0 male  bdi_score_bl      7
    ## 2 10003  48.0 male  bdi_score_01m     1
    ## 3 10003  48.0 male  bdi_score_06m     2
    ## 4 10003  48.0 male  bdi_score_12m     0
    ## 5 10015  72.5 male  bdi_score_bl      6
    ## # … with 4,343 more rows

去掉“vist”值的前缀 This looks much better! However, now visit is an
issue. The original column names were informative but we probably don’t
need to keep the bdi_score\_ prefix in each case. I’ll use an additional
option in pivot_longer to address this:

``` r
pulse_tidy_data = 
  pivot_longer(
    pulse_df, 
    bdi_score_bl:bdi_score_12m,
    names_to = "visit", 
    names_prefix = "bdi_score_", #remove prefix
    values_to = "bdi")

pulse_tidy_data
```

    ## # A tibble: 4,348 × 5
    ##      id   age sex   visit   bdi
    ##   <dbl> <dbl> <chr> <chr> <dbl>
    ## 1 10003  48.0 male  bl        7
    ## 2 10003  48.0 male  01m       1
    ## 3 10003  48.0 male  06m       2
    ## 4 10003  48.0 male  12m       0
    ## 5 10015  72.5 male  bl        6
    ## # … with 4,343 more rows

## using $>$

``` r
pulse_df = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") %>% #load dataset
  janitor::clean_names() %>%  # clean dataset
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to = "visit", 
    names_prefix = "bdi_score_",
    values_to = "bdi") %>% #For tidying single tables
  relocate(visit) %>% #list "visit" to be the first column
  mutate(
    visit = replace(visit, visit == "bl", "00m"), # replace the value "bl" by "00m" in "visit" column
    visit = factor(visit)) %>% # turns "visit" into factor
  arrange(id, visit) #arrange by "id" and "visit"

print(pulse_df, n = 12) #print the dataset with 12 rows
```

    ## # A tibble: 4,348 × 5
    ##    visit    id   age sex     bdi
    ##    <fct> <dbl> <dbl> <chr> <dbl>
    ##  1 00m   10003  48.0 male      7
    ##  2 01m   10003  48.0 male      1
    ##  3 06m   10003  48.0 male      2
    ##  4 12m   10003  48.0 male      0
    ##  5 00m   10015  72.5 male      6
    ##  6 01m   10015  72.5 male     NA
    ##  7 06m   10015  72.5 male     NA
    ##  8 12m   10015  72.5 male     NA
    ##  9 00m   10022  58.5 male     14
    ## 10 01m   10022  58.5 male      3
    ## 11 06m   10022  58.5 male      8
    ## 12 12m   10022  58.5 male     NA
    ## # … with 4,336 more rows

``` r
litters_wide = 
  read_csv("./data/FAS_litters.csv", show_col_types = FALSE) %>%
  janitor::clean_names()
litters_wide
```

    ## # A tibble: 49 × 8
    ##   group litter_number gd0_weight gd18_weight gd_of_birth pups_…¹ pups_…² pups_…³
    ##   <chr> <chr>              <dbl>       <dbl>       <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Con7  #85                 19.7        34.7          20       3       4       3
    ## 2 Con7  #1/2/95/2           27          42            19       8       0       7
    ## 3 Con7  #5/5/3/83/3-3       26          41.4          19       6       0       5
    ## 4 Con7  #5/4/2/95/2         28.5        44.1          19       5       1       4
    ## 5 Con7  #4/2/95/3-3         NA          NA            20       6       0       6
    ## # … with 44 more rows, and abbreviated variable names ¹​pups_born_alive,
    ## #   ²​pups_dead_birth, ³​pups_survive

``` r
litters_wide = 
  read_csv("./data/FAS_litters.csv", show_col_types = FALSE) %>%
  janitor::clean_names()%>%
  select(litter_number, ends_with("weight")) %>% 
  pivot_longer(
    gd0_weight:gd18_weight,
    names_to = "gd", 
    values_to = "weight") %>% 
  mutate(gd = recode(gd, "gd0_weight" = 0, "gd18_weight" = 18))
litters_wide
```

    ## # A tibble: 98 × 3
    ##   litter_number    gd weight
    ##   <chr>         <dbl>  <dbl>
    ## 1 #85               0   19.7
    ## 2 #85              18   34.7
    ## 3 #1/2/95/2         0   27  
    ## 4 #1/2/95/2        18   42  
    ## 5 #5/5/3/83/3-3     0   26  
    ## # … with 93 more rows
