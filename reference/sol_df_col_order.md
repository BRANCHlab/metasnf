# Helper function for organizing solutions df-like column order

Reorders columns of a solutions data frame to "solution", "nclust",
"mc", then all other column names.

## Usage

``` r
sol_df_col_order(x)
```

## Arguments

- x:

  Object with columns "solution", "nclust", and "mc".

## Value

x with column names reordered.
