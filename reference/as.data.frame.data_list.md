# Coerce a `data_list` class object into a `data.frame` class object

Horizontally joins data frames within a data list into a single data
frame, using the `uid` attribute as the joining key.

## Usage

``` r
# S3 method for class 'data_list'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  A `data_list` class object.

- row.names:

  Additional parameter passed to
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- optional:

  Additional parameter passed to
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- ...:

  Additional parameter passed to
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Value

dl_df A `data.frame` class object with all the features and observations
of `dl`.
