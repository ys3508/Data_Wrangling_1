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

load the dataset

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

wide format to long format…

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

去掉“vist”值的前缀 remove prefix

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

rewrite, combine, and extend(to add a mutate)

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

# pivot_wider

make up some data

``` r
analysis_result = tibble(
  group = c("treatment", "treatment", "placebo", "placebo"),
  time = c("pre", "post", "pre", "post"),
  mean = c(4, 8, 3.5, 4)
)

analysis_result
```

    ## # A tibble: 4 × 3
    ##   group     time   mean
    ##   <chr>     <chr> <dbl>
    ## 1 treatment pre     4  
    ## 2 treatment post    8  
    ## 3 placebo   pre     3.5
    ## 4 placebo   post    4

long format to wide format

``` r
pivot_wider(
  analysis_result, 
  names_from = "time", 
  values_from = "mean")
```

    ## # A tibble: 2 × 3
    ##   group       pre  post
    ##   <chr>     <dbl> <dbl>
    ## 1 treatment   4       8
    ## 2 placebo     3.5     4

# Binding rows

import data

``` r
fellowship_ring = 
  readxl::read_excel("./data/LotR_Words.xlsx", range = "B3:D6") %>%
  mutate(movie = "fellowship_ring")
fellowship_ring
```

    ## # A tibble: 3 × 4
    ##   Race   Female  Male movie          
    ##   <chr>   <dbl> <dbl> <chr>          
    ## 1 Elf      1229   971 fellowship_ring
    ## 2 Hobbit     14  3644 fellowship_ring
    ## 3 Man         0  1995 fellowship_ring

``` r
two_towers = 
  readxl::read_excel("./data/LotR_Words.xlsx", range = "F3:H6") %>%
  mutate(movie = "two_towers")

return_king = 
  readxl::read_excel("./data/LotR_Words.xlsx", range = "J3:L6") %>%
  mutate(movie = "return_king")
```

stack datasets together

``` r
lotr_tidy = 
  bind_rows(fellowship_ring, two_towers, return_king) %>%
  janitor::clean_names() %>%
  pivot_longer(
    female:male,
    names_to = "gender", #character vectors
    values_to = "words") %>% #numberic vectors
  mutate(race = str_to_lower(race)) %>% #全换成小写
  select(movie, everything()) #movie在第一列但保留其他columns

lotr_tidy
```

    ## # A tibble: 18 × 4
    ##    movie           race   gender words
    ##    <chr>           <chr>  <chr>  <dbl>
    ##  1 fellowship_ring elf    female  1229
    ##  2 fellowship_ring elf    male     971
    ##  3 fellowship_ring hobbit female    14
    ##  4 fellowship_ring hobbit male    3644
    ##  5 fellowship_ring man    female     0
    ##  6 fellowship_ring man    male    1995
    ##  7 two_towers      elf    female   331
    ##  8 two_towers      elf    male     513
    ##  9 two_towers      hobbit female     0
    ## 10 two_towers      hobbit male    2463
    ## 11 two_towers      man    female   401
    ## 12 two_towers      man    male    3589
    ## 13 return_king     elf    female   183
    ## 14 return_king     elf    male     510
    ## 15 return_king     hobbit female     2
    ## 16 return_king     hobbit male    2673
    ## 17 return_king     man    female   268
    ## 18 return_king     man    male    2459

# Joining datasets

There are four major ways join dataframes x and y:

Inner: keeps data that appear in both x and y Left: keeps data that
appear in x Right: keeps data that appear in y Full: keeps data that
appear in either x or y Left joins are the most common, because they add
data from a smaller table y into a larger table x without removing
anything from x.

import data

``` r
pup_data = 
  read_csv("./data/FAS_pups.csv", show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  mutate(
    sex = recode(sex, `1` = "male", `2` = "female"),
    sex = factor(sex)) 
pup_data
```

    ## # A tibble: 313 × 6
    ##   litter_number sex   pd_ears pd_eyes pd_pivot pd_walk
    ##   <chr>         <fct>   <dbl>   <dbl>    <dbl>   <dbl>
    ## 1 #85           male        4      13        7      11
    ## 2 #85           male        4      13        7      12
    ## 3 #1/2/95/2     male        5      13        7       9
    ## 4 #1/2/95/2     male        5      13        8      10
    ## 5 #5/5/3/83/3-3 male        5      13        8      10
    ## # … with 308 more rows

``` r
litter_data = 
  read_csv("./data/FAS_litters.csv", show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  separate(group, into = c("dose", "day_of_tx"), sep = 3) %>% #把group的con和数字分开
  relocate(litter_number) %>%
  mutate(
    wt_gain = gd18_weight - gd0_weight,
    dose = str_to_lower(dose))
litter_data 
```

    ## # A tibble: 49 × 10
    ##   litter…¹ dose  day_o…² gd0_w…³ gd18_…⁴ gd_of…⁵ pups_…⁶ pups_…⁷ pups_…⁸ wt_gain
    ##   <chr>    <chr> <chr>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 #85      con   7          19.7    34.7      20       3       4       3    15  
    ## 2 #1/2/95… con   7          27      42        19       8       0       7    15  
    ## 3 #5/5/3/… con   7          26      41.4      19       6       0       5    15.4
    ## 4 #5/4/2/… con   7          28.5    44.1      19       5       1       4    15.6
    ## 5 #4/2/95… con   7          NA      NA        20       6       0       6    NA  
    ## # … with 44 more rows, and abbreviated variable names ¹​litter_number,
    ## #   ²​day_of_tx, ³​gd0_weight, ⁴​gd18_weight, ⁵​gd_of_birth, ⁶​pups_born_alive,
    ## #   ⁷​pups_dead_birth, ⁸​pups_survive

left join retains data on each pup dataset and adds data from litter
data into new columns

``` r
fas_data = 
  left_join(pup_data, litter_data, by = "litter_number")

fas_data
```

    ## # A tibble: 313 × 15
    ##   litter_n…¹ sex   pd_ears pd_eyes pd_pi…² pd_walk dose  day_o…³ gd0_w…⁴ gd18_…⁵
    ##   <chr>      <fct>   <dbl>   <dbl>   <dbl>   <dbl> <chr> <chr>     <dbl>   <dbl>
    ## 1 #85        male        4      13       7      11 con   7          19.7    34.7
    ## 2 #85        male        4      13       7      12 con   7          19.7    34.7
    ## 3 #1/2/95/2  male        5      13       7       9 con   7          27      42  
    ## 4 #1/2/95/2  male        5      13       8      10 con   7          27      42  
    ## 5 #5/5/3/83… male        5      13       8      10 con   7          26      41.4
    ## # … with 308 more rows, 5 more variables: gd_of_birth <dbl>,
    ## #   pups_born_alive <dbl>, pups_dead_birth <dbl>, pups_survive <dbl>,
    ## #   wt_gain <dbl>, and abbreviated variable names ¹​litter_number, ²​pd_pivot,
    ## #   ³​day_of_tx, ⁴​gd0_weight, ⁵​gd18_weight

## another example

import two dataset

``` r
surv_os = read_csv("./survey_results/surv_os.csv") %>% 
  janitor::clean_names() %>% 
  rename(id = what_is_your_uni, os = what_operating_system_do_you_use)
```

    ## Rows: 173 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): What is your UNI?, What operating system do you use?
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
surv_os
```

    ## # A tibble: 173 × 2
    ##   id          os        
    ##   <chr>       <chr>     
    ## 1 student_87  <NA>      
    ## 2 student_106 Windows 10
    ## 3 student_66  Mac OS X  
    ## 4 student_93  Windows 10
    ## 5 student_99  Mac OS X  
    ## # … with 168 more rows

``` r
surv_pr_git = read_csv("./survey_results/surv_program_git.csv") %>% 
  janitor::clean_names() %>% 
  rename(
    id = what_is_your_uni, 
    prog = what_is_your_degree_program,
    git_exp = which_most_accurately_describes_your_experience_with_git)
```

    ## Rows: 135 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): What is your UNI?, What is your degree program?, Which most accurat...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
surv_pr_git
```

    ## # A tibble: 135 × 3
    ##   id          prog  git_exp                                                     
    ##   <chr>       <chr> <chr>                                                       
    ## 1 student_133 MS    Pretty smooth: needed some work to connect Git, GitHub, and…
    ## 2 student_32  MS    Not smooth: I don't like git, I don't like GitHub, and I do…
    ## 3 <NA>        MPH   Pretty smooth: needed some work to connect Git, GitHub, and…
    ## 4 student_161 MPH   Not smooth: I don't like git, I don't like GitHub, and I do…
    ## 5 student_17  PhD   Pretty smooth: needed some work to connect Git, GitHub, and…
    ## # … with 130 more rows

join

``` r
left_join(surv_os, surv_pr_git)
```

    ## Joining, by = "id"

    ## # A tibble: 175 × 4
    ##   id          os         prog  git_exp                                          
    ##   <chr>       <chr>      <chr> <chr>                                            
    ## 1 student_87  <NA>       MS    Pretty smooth: needed some work to connect Git, …
    ## 2 student_106 Windows 10 Other Pretty smooth: needed some work to connect Git, …
    ## 3 student_66  Mac OS X   MPH   Smooth: installation and connection with GitHub …
    ## 4 student_93  Windows 10 MS    Smooth: installation and connection with GitHub …
    ## 5 student_99  Mac OS X   MS    Smooth: installation and connection with GitHub …
    ## # … with 170 more rows

``` r
inner_join(surv_os, surv_pr_git)
```

    ## Joining, by = "id"

    ## # A tibble: 129 × 4
    ##   id          os         prog  git_exp                                          
    ##   <chr>       <chr>      <chr> <chr>                                            
    ## 1 student_87  <NA>       MS    Pretty smooth: needed some work to connect Git, …
    ## 2 student_106 Windows 10 Other Pretty smooth: needed some work to connect Git, …
    ## 3 student_66  Mac OS X   MPH   Smooth: installation and connection with GitHub …
    ## 4 student_93  Windows 10 MS    Smooth: installation and connection with GitHub …
    ## 5 student_99  Mac OS X   MS    Smooth: installation and connection with GitHub …
    ## # … with 124 more rows

``` r
anti_join(surv_os, surv_pr_git)
```

    ## Joining, by = "id"

    ## # A tibble: 46 × 2
    ##   id          os        
    ##   <chr>       <chr>     
    ## 1 student_86  Mac OS X  
    ## 2 student_91  Windows 10
    ## 3 student_24  Mac OS X  
    ## 4 student_103 Mac OS X  
    ## 5 student_163 Mac OS X  
    ## # … with 41 more rows

``` r
anti_join(surv_pr_git, surv_os)
```

    ## Joining, by = "id"

    ## # A tibble: 15 × 3
    ##    id         prog  git_exp                                                     
    ##    <chr>      <chr> <chr>                                                       
    ##  1 <NA>       MPH   "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  2 student_17 PhD   "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  3 <NA>       MPH   "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  4 <NA>       MPH   "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  5 <NA>       MS    "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  6 student_53 MS    "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  7 <NA>       MS    "Smooth: installation and connection with GitHub was easy"  
    ##  8 student_80 PhD   "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  9 student_16 MPH   "Smooth: installation and connection with GitHub was easy"  
    ## 10 student_98 MS    "Smooth: installation and connection with GitHub was easy"  
    ## 11 <NA>       MS    "Pretty smooth: needed some work to connect Git, GitHub, an…
    ## 12 <NA>       MS    "What's \"Git\" ...?"                                       
    ## 13 <NA>       MS    "Smooth: installation and connection with GitHub was easy"  
    ## 14 <NA>       MPH   "Pretty smooth: needed some work to connect Git, GitHub, an…
    ## 15 <NA>       MS    "Pretty smooth: needed some work to connect Git, GitHub, an…
