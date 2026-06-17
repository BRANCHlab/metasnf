# Rename features in a data list

Rename features in a data list

## Usage

``` r
rename_dl(dl, name_mapping)
```

## Arguments

- dl:

  A nested list of input data from
  [`data_list()`](https://branchlab.github.io/metasnf/reference/data_list.md).

- name_mapping:

  A named vector where the values are the features to be renamed and the
  names are the new names for those features.

## Value

A data list ("list"-class object) with adjusted feature names.

## Examples

``` r
dl <- data_list(
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    list(depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)
#> ℹ 120 observations dropped due to incomplete data.

summary(dl, "feature")
#>              name       type       domain
#> 1 pubertal_status continuous demographics
#> 2  cbcl_anxiety_r    ordinal    behaviour
#> 3  cbcl_depress_r    ordinal    behaviour

name_changes <- c(
    "anxiety_score" = "cbcl_anxiety_r",
    "depression_score" = "cbcl_depress_r"
)

dl <- rename_dl(dl, name_changes)

summary(dl, "feature")
#>                 Length Class  Mode
#> pubertal_status 4      -none- list
#> anxiety         4      -none- list
#> depressed       4      -none- list
```
