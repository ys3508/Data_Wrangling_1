Data Manipulation
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

# load in the FAS litters data

``` r
litters_df = read_csv("./data/FAS_litters.csv")
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
litters_df = janitor::clean_names(litters_df)
litters_df
```

    ## # A tibble: 49 × 8
    ##    group litter_number   gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>                <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #85                   19.7        34.7       20       3       4       3
    ##  2 Con7  #1/2/95/2             27          42         19       8       0       7
    ##  3 Con7  #5/5/3/83/3-3         26          41.4       19       6       0       5
    ##  4 Con7  #5/4/2/95/2           28.5        44.1       19       5       1       4
    ##  5 Con7  #4/2/95/3-3           NA          NA         20       6       0       6
    ##  6 Con7  #2/2/95/3-2           NA          NA         20       6       0       4
    ##  7 Con7  #1/5/3/83/3-3/2       NA          NA         20       9       0       9
    ##  8 Con8  #3/83/3-3             NA          NA         20       9       1       8
    ##  9 Con8  #2/95/3               NA          NA         20       8       0       8
    ## 10 Con8  #3/5/2/2/95           28.5        NA         20       8       0       8
    ## # … with 39 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

# ‘select’

## choose/remove some columns

``` r
select(litters_df, group, gd0_weight)
```

    ## # A tibble: 49 × 2
    ##    group gd0_weight
    ##    <chr>      <dbl>
    ##  1 Con7        19.7
    ##  2 Con7        27  
    ##  3 Con7        26  
    ##  4 Con7        28.5
    ##  5 Con7        NA  
    ##  6 Con7        NA  
    ##  7 Con7        NA  
    ##  8 Con8        NA  
    ##  9 Con8        NA  
    ## 10 Con8        28.5
    ## # … with 39 more rows

``` r
select(litters_df, - gd0_weight)
```

    ## # A tibble: 49 × 7
    ##    group litter_number   gd18_weight gd_of_birth pups_born_alive pups_…¹ pups_…²
    ##    <chr> <chr>                 <dbl>       <dbl>           <dbl>   <dbl>   <dbl>
    ##  1 Con7  #85                    34.7          20               3       4       3
    ##  2 Con7  #1/2/95/2              42            19               8       0       7
    ##  3 Con7  #5/5/3/83/3-3          41.4          19               6       0       5
    ##  4 Con7  #5/4/2/95/2            44.1          19               5       1       4
    ##  5 Con7  #4/2/95/3-3            NA            20               6       0       6
    ##  6 Con7  #2/2/95/3-2            NA            20               6       0       4
    ##  7 Con7  #1/5/3/83/3-3/2        NA            20               9       0       9
    ##  8 Con8  #3/83/3-3              NA            20               9       1       8
    ##  9 Con8  #2/95/3                NA            20               8       0       8
    ## 10 Con8  #3/5/2/2/95            NA            20               8       0       8
    ## # … with 39 more rows, and abbreviated variable names ¹​pups_dead_birth,
    ## #   ²​pups_survive

## selct a range column

``` r
select(litters_df, group, gd0_weight:gd_of_birth)
```

    ## # A tibble: 49 × 4
    ##    group gd0_weight gd18_weight gd_of_birth
    ##    <chr>      <dbl>       <dbl>       <dbl>
    ##  1 Con7        19.7        34.7          20
    ##  2 Con7        27          42            19
    ##  3 Con7        26          41.4          19
    ##  4 Con7        28.5        44.1          19
    ##  5 Con7        NA          NA            20
    ##  6 Con7        NA          NA            20
    ##  7 Con7        NA          NA            20
    ##  8 Con8        NA          NA            20
    ##  9 Con8        NA          NA            20
    ## 10 Con8        28.5        NA            20
    ## # … with 39 more rows

## renaming columns

``` r
# in one step
select(litters_df, GROUP = group, LITTER_NUMBER = litter_number)
```

    ## # A tibble: 49 × 2
    ##    GROUP LITTER_NUMBER  
    ##    <chr> <chr>          
    ##  1 Con7  #85            
    ##  2 Con7  #1/2/95/2      
    ##  3 Con7  #5/5/3/83/3-3  
    ##  4 Con7  #5/4/2/95/2    
    ##  5 Con7  #4/2/95/3-3    
    ##  6 Con7  #2/2/95/3-2    
    ##  7 Con7  #1/5/3/83/3-3/2
    ##  8 Con8  #3/83/3-3      
    ##  9 Con8  #2/95/3        
    ## 10 Con8  #3/5/2/2/95    
    ## # … with 39 more rows

``` r
# in multiple steps
rename(litters_df, GROUP = group, LITTER_NUMBER = litter_number)
```

    ## # A tibble: 49 × 8
    ##    GROUP LITTER_NUMBER   gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>                <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #85                   19.7        34.7       20       3       4       3
    ##  2 Con7  #1/2/95/2             27          42         19       8       0       7
    ##  3 Con7  #5/5/3/83/3-3         26          41.4       19       6       0       5
    ##  4 Con7  #5/4/2/95/2           28.5        44.1       19       5       1       4
    ##  5 Con7  #4/2/95/3-3           NA          NA         20       6       0       6
    ##  6 Con7  #2/2/95/3-2           NA          NA         20       6       0       4
    ##  7 Con7  #1/5/3/83/3-3/2       NA          NA         20       9       0       9
    ##  8 Con8  #3/83/3-3             NA          NA         20       9       1       8
    ##  9 Con8  #2/95/3               NA          NA         20       8       0       8
    ## 10 Con8  #3/5/2/2/95           28.5        NA         20       8       0       8
    ## # … with 39 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

## select helpers

下面打?select_helpers

## select using keywords

``` r
select(litters_df, starts_with("gd"))
```

    ## # A tibble: 49 × 3
    ##    gd0_weight gd18_weight gd_of_birth
    ##         <dbl>       <dbl>       <dbl>
    ##  1       19.7        34.7          20
    ##  2       27          42            19
    ##  3       26          41.4          19
    ##  4       28.5        44.1          19
    ##  5       NA          NA            20
    ##  6       NA          NA            20
    ##  7       NA          NA            20
    ##  8       NA          NA            20
    ##  9       NA          NA            20
    ## 10       28.5        NA            20
    ## # … with 39 more rows

``` r
select(litters_df, ends_with("ht"))
```

    ## # A tibble: 49 × 2
    ##    gd0_weight gd18_weight
    ##         <dbl>       <dbl>
    ##  1       19.7        34.7
    ##  2       27          42  
    ##  3       26          41.4
    ##  4       28.5        44.1
    ##  5       NA          NA  
    ##  6       NA          NA  
    ##  7       NA          NA  
    ##  8       NA          NA  
    ##  9       NA          NA  
    ## 10       28.5        NA  
    ## # … with 39 more rows

``` r
select(litters_df, contains("ht"))
```

    ## # A tibble: 49 × 2
    ##    gd0_weight gd18_weight
    ##         <dbl>       <dbl>
    ##  1       19.7        34.7
    ##  2       27          42  
    ##  3       26          41.4
    ##  4       28.5        44.1
    ##  5       NA          NA  
    ##  6       NA          NA  
    ##  7       NA          NA  
    ##  8       NA          NA  
    ##  9       NA          NA  
    ## 10       28.5        NA  
    ## # … with 39 more rows

## sequencing

把litter_number放在最前面但是保留其他的columns everything(), which is
handy for reorganizing columns without discarding anything

``` r
select(litters_df, litter_number, everything())
```

    ## # A tibble: 49 × 8
    ##    litter_number   group gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr>           <chr>      <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 #85             Con7        19.7        34.7       20       3       4       3
    ##  2 #1/2/95/2       Con7        27          42         19       8       0       7
    ##  3 #5/5/3/83/3-3   Con7        26          41.4       19       6       0       5
    ##  4 #5/4/2/95/2     Con7        28.5        44.1       19       5       1       4
    ##  5 #4/2/95/3-3     Con7        NA          NA         20       6       0       6
    ##  6 #2/2/95/3-2     Con7        NA          NA         20       6       0       4
    ##  7 #1/5/3/83/3-3/2 Con7        NA          NA         20       9       0       9
    ##  8 #3/83/3-3       Con8        NA          NA         20       9       1       8
    ##  9 #2/95/3         Con8        NA          NA         20       8       0       8
    ## 10 #3/5/2/2/95     Con8        28.5        NA         20       8       0       8
    ## # … with 39 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

OR

``` r
relocate(litters_df, litter_number)
```

    ## # A tibble: 49 × 8
    ##    litter_number   group gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr>           <chr>      <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 #85             Con7        19.7        34.7       20       3       4       3
    ##  2 #1/2/95/2       Con7        27          42         19       8       0       7
    ##  3 #5/5/3/83/3-3   Con7        26          41.4       19       6       0       5
    ##  4 #5/4/2/95/2     Con7        28.5        44.1       19       5       1       4
    ##  5 #4/2/95/3-3     Con7        NA          NA         20       6       0       6
    ##  6 #2/2/95/3-2     Con7        NA          NA         20       6       0       4
    ##  7 #1/5/3/83/3-3/2 Con7        NA          NA         20       9       0       9
    ##  8 #3/83/3-3       Con8        NA          NA         20       9       1       8
    ##  9 #2/95/3         Con8        NA          NA         20       8       0       8
    ## 10 #3/5/2/2/95     Con8        28.5        NA         20       8       0       8
    ## # … with 39 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

# ‘filter’

You will often filter using comparison operators (\>, \>=, \<, \<=, ==,
and !=). You may also use %in% to detect if values appear in a set, and
is.na() to find missing values. The results of comparisons are logical –
the statement is TRUE or FALSE depending on the values you compare – and
can be combined with other comparisons using the logical operators & and
\|, or negated using !.

与select不同点： 1. select 处理column; filter处理rows 2. fiilter
有conditioners, based on ture/false, filter rows

## remove/keep rows

``` r
filter(litters_df, gd_of_birth < 22)
```

    ## # A tibble: 49 × 8
    ##    group litter_number   gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>                <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #85                   19.7        34.7       20       3       4       3
    ##  2 Con7  #1/2/95/2             27          42         19       8       0       7
    ##  3 Con7  #5/5/3/83/3-3         26          41.4       19       6       0       5
    ##  4 Con7  #5/4/2/95/2           28.5        44.1       19       5       1       4
    ##  5 Con7  #4/2/95/3-3           NA          NA         20       6       0       6
    ##  6 Con7  #2/2/95/3-2           NA          NA         20       6       0       4
    ##  7 Con7  #1/5/3/83/3-3/2       NA          NA         20       9       0       9
    ##  8 Con8  #3/83/3-3             NA          NA         20       9       1       8
    ##  9 Con8  #2/95/3               NA          NA         20       8       0       8
    ## 10 Con8  #3/5/2/2/95           28.5        NA         20       8       0       8
    ## # … with 39 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

``` r
filter(litters_df, gd_of_birth == 20)
```

    ## # A tibble: 32 × 8
    ##    group litter_number   gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>                <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #85                   19.7        34.7       20       3       4       3
    ##  2 Con7  #4/2/95/3-3           NA          NA         20       6       0       6
    ##  3 Con7  #2/2/95/3-2           NA          NA         20       6       0       4
    ##  4 Con7  #1/5/3/83/3-3/2       NA          NA         20       9       0       9
    ##  5 Con8  #3/83/3-3             NA          NA         20       9       1       8
    ##  6 Con8  #2/95/3               NA          NA         20       8       0       8
    ##  7 Con8  #3/5/2/2/95           28.5        NA         20       8       0       8
    ##  8 Con8  #1/6/2/2/95-2         NA          NA         20       7       0       6
    ##  9 Con8  #3/5/3/83/3-3-2       NA          NA         20       8       0       8
    ## 10 Con8  #3/6/2/2/95-3         NA          NA         20       7       0       7
    ## # … with 22 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

``` r
filter(litters_df, !(gd_of_birth == 20)) # flip the result
```

    ## # A tibble: 17 × 8
    ##    group litter_number gd0_weight gd18_weight gd_of_bi…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>              <dbl>       <dbl>      <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #1/2/95/2           27          42           19       8       0       7
    ##  2 Con7  #5/5/3/83/3-3       26          41.4         19       6       0       5
    ##  3 Con7  #5/4/2/95/2         28.5        44.1         19       5       1       4
    ##  4 Con8  #5/4/3/83/3         28          NA           19       9       0       8
    ##  5 Con8  #2/2/95/2           NA          NA           19       5       0       4
    ##  6 Mod7  #59                 17          33.4         19       8       0       5
    ##  7 Mod7  #103                21.4        42.1         19       9       1       9
    ##  8 Mod7  #1/82/3-2           NA          NA           19       6       0       6
    ##  9 Mod7  #3/83/3-2           NA          NA           19       8       0       8
    ## 10 Mod7  #4/2/95/2           23.5        NA           19       9       0       7
    ## 11 Mod7  #5/3/83/5-2         22.6        37           19       5       0       5
    ## 12 Mod7  #94/2               24.4        42.9         19       7       1       3
    ## 13 Mod7  #62                 19.5        35.9         19       7       2       4
    ## 14 Low7  #112                23.9        40.5         19       6       1       1
    ## 15 Mod8  #5/93/2             NA          NA           19       8       0       8
    ## 16 Mod8  #7/110/3-2          27.5        46           19       8       1       8
    ## 17 Low8  #79                 25.4        43.8         19       8       0       7
    ## # … with abbreviated variable names ¹​gd_of_birth, ²​pups_born_alive,
    ## #   ³​pups_dead_birth, ⁴​pups_survive

``` r
filter(litters_df,  group %in% c("Con7", "Con8")) # keep groups = con7 or con8 
```

    ## # A tibble: 15 × 8
    ##    group litter_number   gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>                <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #85                   19.7        34.7       20       3       4       3
    ##  2 Con7  #1/2/95/2             27          42         19       8       0       7
    ##  3 Con7  #5/5/3/83/3-3         26          41.4       19       6       0       5
    ##  4 Con7  #5/4/2/95/2           28.5        44.1       19       5       1       4
    ##  5 Con7  #4/2/95/3-3           NA          NA         20       6       0       6
    ##  6 Con7  #2/2/95/3-2           NA          NA         20       6       0       4
    ##  7 Con7  #1/5/3/83/3-3/2       NA          NA         20       9       0       9
    ##  8 Con8  #3/83/3-3             NA          NA         20       9       1       8
    ##  9 Con8  #2/95/3               NA          NA         20       8       0       8
    ## 10 Con8  #3/5/2/2/95           28.5        NA         20       8       0       8
    ## 11 Con8  #5/4/3/83/3           28          NA         19       9       0       8
    ## 12 Con8  #1/6/2/2/95-2         NA          NA         20       7       0       6
    ## 13 Con8  #3/5/3/83/3-3-2       NA          NA         20       8       0       8
    ## 14 Con8  #2/2/95/2             NA          NA         19       5       0       4
    ## 15 Con8  #3/6/2/2/95-3         NA          NA         20       7       0       7
    ## # … with abbreviated variable names ¹​gd_of_birth, ²​pups_born_alive,
    ## #   ³​pups_dead_birth, ⁴​pups_survive

``` r
filter(litters_df,  group == "Con7" & gd_of_birth == 20)
```

    ## # A tibble: 4 × 8
    ##   group litter_number   gd0_weight gd18_weight gd_of_b…¹ pups_…² pups_…³ pups_…⁴
    ##   <chr> <chr>                <dbl>       <dbl>     <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Con7  #85                   19.7        34.7        20       3       4       3
    ## 2 Con7  #4/2/95/3-3           NA          NA          20       6       0       6
    ## 3 Con7  #2/2/95/3-2           NA          NA          20       6       0       4
    ## 4 Con7  #1/5/3/83/3-3/2       NA          NA          20       9       0       9
    ## # … with abbreviated variable names ¹​gd_of_birth, ²​pups_born_alive,
    ## #   ³​pups_dead_birth, ⁴​pups_survive

## filter and NA

Remove All NAS in the whole dataset

``` r
drop_na(litters_df)
```

    ## # A tibble: 31 × 8
    ##    group litter_number gd0_weight gd18_weight gd_of_bi…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>              <dbl>       <dbl>      <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #85                 19.7        34.7         20       3       4       3
    ##  2 Con7  #1/2/95/2           27          42           19       8       0       7
    ##  3 Con7  #5/5/3/83/3-3       26          41.4         19       6       0       5
    ##  4 Con7  #5/4/2/95/2         28.5        44.1         19       5       1       4
    ##  5 Mod7  #59                 17          33.4         19       8       0       5
    ##  6 Mod7  #103                21.4        42.1         19       9       1       9
    ##  7 Mod7  #3/82/3-2           28          45.9         20       5       0       5
    ##  8 Mod7  #5/3/83/5-2         22.6        37           19       5       0       5
    ##  9 Mod7  #106                21.7        37.8         20       5       0       2
    ## 10 Mod7  #94/2               24.4        42.9         19       7       1       3
    ## # … with 21 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

Remove NAS only in column “gd0_weight”

``` r
drop_na(litters_df, gd0_weight) 
```

    ## # A tibble: 34 × 8
    ##    group litter_number gd0_weight gd18_weight gd_of_bi…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>              <dbl>       <dbl>      <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #85                 19.7        34.7         20       3       4       3
    ##  2 Con7  #1/2/95/2           27          42           19       8       0       7
    ##  3 Con7  #5/5/3/83/3-3       26          41.4         19       6       0       5
    ##  4 Con7  #5/4/2/95/2         28.5        44.1         19       5       1       4
    ##  5 Con8  #3/5/2/2/95         28.5        NA           20       8       0       8
    ##  6 Con8  #5/4/3/83/3         28          NA           19       9       0       8
    ##  7 Mod7  #59                 17          33.4         19       8       0       5
    ##  8 Mod7  #103                21.4        42.1         19       9       1       9
    ##  9 Mod7  #3/82/3-2           28          45.9         20       5       0       5
    ## 10 Mod7  #4/2/95/2           23.5        NA           19       9       0       7
    ## # … with 24 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

# ‘mutate’

modify old variables or create new variables

``` r
mutate(litters_df,   wt_gain = gd18_weight - gd0_weight, #muate a new variable#
  group = str_to_lower(group)) #modify the "group" variable with all lower cases
```

    ## # A tibble: 49 × 9
    ##    group litter_number   gd0_w…¹ gd18_…² gd_of…³ pups_…⁴ pups_…⁵ pups_…⁶ wt_gain
    ##    <chr> <chr>             <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 con7  #85                19.7    34.7      20       3       4       3    15  
    ##  2 con7  #1/2/95/2          27      42        19       8       0       7    15  
    ##  3 con7  #5/5/3/83/3-3      26      41.4      19       6       0       5    15.4
    ##  4 con7  #5/4/2/95/2        28.5    44.1      19       5       1       4    15.6
    ##  5 con7  #4/2/95/3-3        NA      NA        20       6       0       6    NA  
    ##  6 con7  #2/2/95/3-2        NA      NA        20       6       0       4    NA  
    ##  7 con7  #1/5/3/83/3-3/2    NA      NA        20       9       0       9    NA  
    ##  8 con8  #3/83/3-3          NA      NA        20       9       1       8    NA  
    ##  9 con8  #2/95/3            NA      NA        20       8       0       8    NA  
    ## 10 con8  #3/5/2/2/95        28.5    NA        20       8       0       8    NA  
    ## # … with 39 more rows, and abbreviated variable names ¹​gd0_weight,
    ## #   ²​gd18_weight, ³​gd_of_birth, ⁴​pups_born_alive, ⁵​pups_dead_birth,
    ## #   ⁶​pups_survive

# ‘arrange’

put things in orders

``` r
head(arrange(litters_df, group, pups_born_alive), 10) # show first 10 rows
```

    ## # A tibble: 10 × 8
    ##    group litter_number   gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>                <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #85                   19.7        34.7       20       3       4       3
    ##  2 Con7  #5/4/2/95/2           28.5        44.1       19       5       1       4
    ##  3 Con7  #5/5/3/83/3-3         26          41.4       19       6       0       5
    ##  4 Con7  #4/2/95/3-3           NA          NA         20       6       0       6
    ##  5 Con7  #2/2/95/3-2           NA          NA         20       6       0       4
    ##  6 Con7  #1/2/95/2             27          42         19       8       0       7
    ##  7 Con7  #1/5/3/83/3-3/2       NA          NA         20       9       0       9
    ##  8 Con8  #2/2/95/2             NA          NA         19       5       0       4
    ##  9 Con8  #1/6/2/2/95-2         NA          NA         20       7       0       6
    ## 10 Con8  #3/6/2/2/95-3         NA          NA         20       7       0       7
    ## # … with abbreviated variable names ¹​gd_of_birth, ²​pups_born_alive,
    ## #   ³​pups_dead_birth, ⁴​pups_survive

``` r
# arrange the litters_df by group and pups_born_alive
```

# ‘%\>%’ data processing

## bad example

``` r
litters_data_raw = read_csv("./data/FAS_litters.csv",
  col_types = "ccddiiii")
litters_data_clean_names = janitor::clean_names(litters_data_raw)
litters_data_selected_cols = select(litters_data_clean_names, -pups_survive)
litters_data_with_vars = 
  mutate(
    litters_data_selected_cols, 
    wt_gain = gd18_weight - gd0_weight,
    group = str_to_lower(group))
litters_data_with_vars_without_missing = 
  drop_na(litters_data_with_vars, wt_gain)
litters_data_with_vars_without_missing
```

    ## # A tibble: 31 × 8
    ##    group litter_number gd0_weight gd18_weight gd_of_bi…¹ pups_…² pups_…³ wt_gain
    ##    <chr> <chr>              <dbl>       <dbl>      <int>   <int>   <int>   <dbl>
    ##  1 con7  #85                 19.7        34.7         20       3       4    15  
    ##  2 con7  #1/2/95/2           27          42           19       8       0    15  
    ##  3 con7  #5/5/3/83/3-3       26          41.4         19       6       0    15.4
    ##  4 con7  #5/4/2/95/2         28.5        44.1         19       5       1    15.6
    ##  5 mod7  #59                 17          33.4         19       8       0    16.4
    ##  6 mod7  #103                21.4        42.1         19       9       1    20.7
    ##  7 mod7  #3/82/3-2           28          45.9         20       5       0    17.9
    ##  8 mod7  #5/3/83/5-2         22.6        37           19       5       0    14.4
    ##  9 mod7  #106                21.7        37.8         20       5       0    16.1
    ## 10 mod7  #94/2               24.4        42.9         19       7       1    18.5
    ## # … with 21 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth

``` r
litters_data_clean = 
  drop_na(
    mutate(
      select(
        janitor::clean_names(
          read_csv("./data/FAS_litters.csv", col_types = "ccddiiii")
          ), 
      -pups_survive
      ),
    wt_gain = gd18_weight - gd0_weight,
    group = str_to_lower(group)
    ),
  wt_gain
  )
```

## Using %\>%

%\>% = “then” %\>% 快捷键：Cmd + Shift + M

``` r
litters_data = 
  read_csv("./data/FAS_litters.csv", col_types = "ccddiiii") %>%
  janitor::clean_names() %>%
  select(-pups_survive) %>%
  mutate(
    wt_gain = gd18_weight - gd0_weight,
    group = str_to_lower(group)) %>% 
  drop_na(wt_gain)

litters_data
```

    ## # A tibble: 31 × 8
    ##    group litter_number gd0_weight gd18_weight gd_of_bi…¹ pups_…² pups_…³ wt_gain
    ##    <chr> <chr>              <dbl>       <dbl>      <int>   <int>   <int>   <dbl>
    ##  1 con7  #85                 19.7        34.7         20       3       4    15  
    ##  2 con7  #1/2/95/2           27          42           19       8       0    15  
    ##  3 con7  #5/5/3/83/3-3       26          41.4         19       6       0    15.4
    ##  4 con7  #5/4/2/95/2         28.5        44.1         19       5       1    15.6
    ##  5 mod7  #59                 17          33.4         19       8       0    16.4
    ##  6 mod7  #103                21.4        42.1         19       9       1    20.7
    ##  7 mod7  #3/82/3-2           28          45.9         20       5       0    17.9
    ##  8 mod7  #5/3/83/5-2         22.6        37           19       5       0    14.4
    ##  9 mod7  #106                21.7        37.8         20       5       0    16.1
    ## 10 mod7  #94/2               24.4        42.9         19       7       1    18.5
    ## # … with 21 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth

``` r
read_csv("./data/FAS_pups.csv", col_types = "ciiiii") %>%
  janitor::clean_names() %>% 
  filter(sex == 1) %>% 
  select(-pd_ears) %>% 
  mutate(pd_pivot_gt7 = pd_pivot > 7)
```

    ## # A tibble: 155 × 6
    ##    litter_number   sex pd_eyes pd_pivot pd_walk pd_pivot_gt7
    ##    <chr>         <int>   <int>    <int>   <int> <lgl>       
    ##  1 #85               1      13        7      11 FALSE       
    ##  2 #85               1      13        7      12 FALSE       
    ##  3 #1/2/95/2         1      13        7       9 FALSE       
    ##  4 #1/2/95/2         1      13        8      10 TRUE        
    ##  5 #5/5/3/83/3-3     1      13        8      10 TRUE        
    ##  6 #5/5/3/83/3-3     1      14        6       9 FALSE       
    ##  7 #5/4/2/95/2       1      14        5       9 FALSE       
    ##  8 #4/2/95/3-3       1      13        6       8 FALSE       
    ##  9 #4/2/95/3-3       1      13        7       9 FALSE       
    ## 10 #2/2/95/3-2       1      NA        8      10 TRUE        
    ## # … with 145 more rows

## Using . as placeholder

You can make this more explicit by using . as a placeholder for the
result of the preceding call

``` r
litters_data = 
  read_csv("./data/FAS_litters.csv", col_types = "ccddiiii") %>%
  janitor::clean_names(dat = .) %>%
  select(.data = ., -pups_survive) %>%
  mutate(.data = .,
    wt_gain = gd18_weight - gd0_weight,
    group = str_to_lower(group)) %>% 
  drop_na(data = ., wt_gain)
```
