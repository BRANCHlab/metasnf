# Calculate p-values based on feature vectors and their types

Calculate p-values based on feature vectors and their types

## Usage

``` r
calc_assoc_pval(var1, var2, type1, type2, cat_test = "chi_squared")
```

## Arguments

- var1:

  A single vector containing a feature.

- var2:

  A single vector containing a feature.

- type1:

  The type of var1 (continuous, discrete, ordinal, categorical).

- type2:

  The type of var2 (continuous, discrete, ordinal, categorical).

- cat_test:

  String indicating which statistical test will be used to associate
  cluster with a categorical feature. Options are "chi_squared" for the
  Chi-squared test and "fisher_exact" for Fisher's exact test.

## Value

pval A p-value from a statistical test based on the provided types.
Currently, this will either be the F-test p-value from a linear model if
at least one feature is non-categorical, or the chi-squared test p-value
if both features are categorical.
