# Check valid item names for a data list-like list

Error if data list-like structure doesn't have nested names of "data",
"name", "domain", and "type".

## Usage

``` r
check_dll_subitem_names(dll)
```

## Arguments

- dll:

  A data list-like `list` class object.

## Value

Raises error if dll doesn't have only 4-item nested lists
