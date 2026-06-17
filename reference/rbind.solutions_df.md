# Row-binding of solutions data frame class objects

Row-binding of solutions data frame class objects

## Usage

``` r
# S3 method for class 'solutions_df'
rbind(..., reset_indices = FALSE)
```

## Arguments

- ...:

  An arbitrary number of `solutions_df` class objects.

- reset_indices:

  If TRUE, re-labels the "solutions" indices in the solutions data frame
  from 1 to the number of defined settings.

## Value

A `solutions_df` class object.
