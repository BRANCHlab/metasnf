# Helper function for deprecated function warnings

Helper function for deprecated function warnings

## Usage

``` r
metasnf_deprecated(version, alternative, env = 1)
```

## Arguments

- version:

  Version of `metasnf` in which function has been deprecated.

- alternative:

  Recommended alternative approach.

- env:

  Environment to evaluate expressions in.

## Value

Returns no value. Raises a warning through cli::cli_warn.
