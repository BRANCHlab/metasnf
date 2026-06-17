# Merge list of data frames into a single data frame

This helper function combines all data frames in a single-level list
into a single data frame.

## Usage

``` r
merge_df_list(df_list, join = "inner", uid = "uid", no_na = FALSE)
```

## Arguments

- df_list:

  list of data frames.

- join:

  String indicating if join should be "inner" or "full".

- uid:

  Column name to join on. Default is "uid".

- no_na:

  Whether to remove NA values from the merged data frame.

## Value

Inner join of all data frames in list.

## Examples

``` r
merge_df_list(list(income, pubertal), uid = "unique_id")
#> # A tibble: 275 × 3
#>    unique_id        household_income pubertal_status
#>    <chr>                       <dbl>           <dbl>
#>  1 NDAR_INV0567T2Y9                3            1.33
#>  2 NDAR_INV0GLZNC2W               NA            1   
#>  3 NDAR_INV0IZ157F8                1            2.33
#>  4 NDAR_INV0J4PYA5F                2            3   
#>  5 NDAR_INV0OYE291Q                1          NaN   
#>  6 NDAR_INV0SM1JLXQ               NA          NaN   
#>  7 NDAR_INV0Z87UJDR                3          NaN   
#>  8 NDAR_INV10OMKVLE                1            1   
#>  9 NDAR_INV15FPCW4O                2            1   
#> 10 NDAR_INV19NB4RJK                1            2   
#> # ℹ 265 more rows
```
