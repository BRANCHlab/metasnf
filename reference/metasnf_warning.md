# Helper function for raising warnings

Helper function for raising warnings

## Usage

``` r
metasnf_warning(..., env = 1)
```

## Arguments

- ...:

  Arbitrary number of strings to be pasted together into warning
  message.

- env:

  Environment to evaluate expressions in.

## Value

Returns no value. Raises a warning through cli::cli_warn
