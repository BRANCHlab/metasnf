# Get p-values from an extended solutions data frame

This function can be used to neatly format the p-values associated with
an extended solutions data frame. It can also calculate the negative
logs of those p-values to make it easier to interpret large-scale
differences.

## Usage

``` r
get_pvals(ext_sol_df, negative_log = FALSE, keep_summaries = TRUE)
```

## Arguments

- ext_sol_df:

  The output of `extend_solutions`. A data frame that contains at least
  one p-value column ending in "\_pval".

- negative_log:

  If TRUE, will replace p-values with negative log p-values.

- keep_summaries:

  If FALSE, will remove the mean, min, and max p-value.

## Value

A "data.frame" class object Of only the p-value related columns of the
provided ext_sol_df.
