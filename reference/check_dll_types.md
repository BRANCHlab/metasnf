# Error if data list-like structure has invalid feature types

Error if data list-like structure has invalid feature types

## Usage

``` r
check_dll_types(dll)
```

## Arguments

- dll:

  A data list-like `list` class object.

## Value

Raises an error if the loaded types are not among continuous, discrete,
ordinal, categorical, or mixed.
