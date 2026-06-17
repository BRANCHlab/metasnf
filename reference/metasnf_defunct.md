# Helper function for defunct function errors

Helper function for defunct function errors

## Usage

``` r
metasnf_defunct(version, alternative, env = 1)
```

## Arguments

- version:

  Version of `metasnf` in which function has been made defunct.

- alternative:

  Recommended alternative approach.

- env:

  Environment to evaluate expressions in.

## Value

Returns no value. Raises an error through cli::cli_abort.
