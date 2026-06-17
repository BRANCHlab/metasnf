# Helper function for raising alerts

Helper function for raising alerts

## Usage

``` r
metasnf_alert(..., env = 1)
```

## Arguments

- ...:

  Arbitrary number of strings to be pasted together into alert message.

- env:

  Environment to evaluate expressions in.

## Value

Returns no value. Raises an alert through cli::cli_alert_info.
