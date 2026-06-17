# Merge method for SNF config objects

Merge method for SNF config objects

## Usage

``` r
# S3 method for class 'snf_config'
merge(x, y, reset_indices = TRUE, ...)
```

## Arguments

- x:

  SNF config to merge.

- y:

  SNF config to merge.

- reset_indices:

  If TRUE (default), re-labels the "solutions" indices in the config
  from 1 to the number of defined settings.

- ...:

  Additional arguments passed into merge function.

## Value

An SNF config combining the rows of both prior configurations.
