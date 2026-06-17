# Add columns to a data frame

Add new columns to a data frame by specifying their names and a value to
initialize them with.

## Usage

``` r
add_columns(df, cols, value = NA)
```

## Arguments

- df:

  The data frame to extend.

- cols:

  The vector containing new column names.

- value:

  The values stored in the newly added columns. NA by default.

## Value

A data frame containing with the same columns as the `df` argument as
well as the new columns specified in the `cols` argument.
