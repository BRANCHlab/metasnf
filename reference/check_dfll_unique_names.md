# Check if names in a distance metrics list-like object are unique

Check if names in a distance metrics list-like object are unique

## Usage

``` r
check_dfll_unique_names(dfll)
```

## Arguments

- dfll:

  A distance metrics list-like list object to be validated.

## Value

Doesn't return any value. Raises error if the items of dfll aren't
unique across layer 1 or within each item of layer 2.
