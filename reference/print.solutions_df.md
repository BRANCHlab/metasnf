# Print method for class `solutions_df`

Custom formatted print for weights matrices that outputs information
about feature weights functions to the console.

## Usage

``` r
# S3 method for class 'solutions_df'
print(x, n = NULL, tips = TRUE, ...)
```

## Arguments

- x:

  A `weights_matrix` class object.

- n:

  Number of rows to print, passed into
  [`tibble::print.tbl_df()`](https://tibble.tidyverse.org/reference/formatting.html).

- tips:

  If TRUE, include lines on how to print more rows / transposed.

- ...:

  Other arguments passed to `print` (not used in this function).

## Value

Function prints to console but does not return any value.
