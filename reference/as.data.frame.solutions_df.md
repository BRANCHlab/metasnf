# Coerce a `solutions_df` class object into a `data.frame` class object

Coerce a `solutions_df` class object into a `data.frame` class object

## Usage

``` r
# S3 method for class 'solutions_df'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  keep_attributes = FALSE,
  ...
)
```

## Arguments

- x:

  A `solutions_df` class object.

- row.names:

  Additional parameter passed to
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- optional:

  Additional parameter passed to
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- keep_attributes:

  If TRUE, resulting data frame includes settings data frame and weights
  matrix.

- ...:

  Additional parameter passed to
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Value

A `data.frame` class object with all the columns of x and its contained
solutions data frame.
