# Calculate p-values for all pairwise associations of features in a data list

Calculate p-values for all pairwise associations of features in a data
list

## Usage

``` r
calc_assoc_pval_matrix(dl, verbose = FALSE, cat_test = "chi_squared")
```

## Arguments

- dl:

  A nested list of input data from
  [`data_list()`](https://branchlab.github.io/metasnf/reference/data_list.md).

- verbose:

  If TRUE, output progress to the console.

- cat_test:

  String indicating which statistical test will be used to associate
  cluster with a categorical feature. Options are "chi_squared" for the
  Chi-squared test and "fisher_exact" for Fisher's exact test.

## Value

A "matrix" class object containing pairwise association p-values between
the features in the provided data list.

## Examples

``` r
data_list <- data_list(
    list(income, "household_income", "demographics", "ordinal"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    list(depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)
#> ℹ 139 observations dropped due to incomplete data.

assoc_pval_matrix <- calc_assoc_pval_matrix(data_list)
```
