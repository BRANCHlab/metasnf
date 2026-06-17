# Merge observations between two compatible data lists

Join two data lists with the same components (data frames) but separate
observations. To instead merge two data lists that have the same
observations but different components, simply use
[`c()`](https://rdrr.io/r/base/c.html).

## Usage

``` r
# S3 method for class 'data_list'
merge(x, y, ...)
```

## Arguments

- x:

  The first data list to merge.

- y:

  The second data list to merge.

- ...:

  Additional arguments passed into merge function.

## Value

A data list ("list"-class object) containing the observations of both
provided data lists.
