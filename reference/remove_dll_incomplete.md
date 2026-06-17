# Remove observations with incomplete data from a data list-like list object

Helper function during `data_list` class initialization. First applies
[`stats::na.omit()`](https://rdrr.io/r/stats/na.fail.html) to the data
frames named "data" within a nested list. Then removes any observations
that are not present across all data frames.

## Usage

``` r
remove_dll_incomplete(dll)
```

## Arguments

- dll:

  A data list-like `list` class object.

## Value

The provided dll with missing observations removed.
