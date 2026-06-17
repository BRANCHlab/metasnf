# Execute inclusion

Given a data list and a settings data frame row, returns a data list of
selected inputs.

## Usage

``` r
drop_inputs(sdf_row, dl)
```

## Arguments

- sdf_row:

  Row of a settings data frame.

- dl:

  A nested list of input data from
  [`data_list()`](https://branchlab.github.io/metasnf/reference/data_list.md).

## Value

A data list (class "list") in which any component with a corresponding 0
value in the provided settings data frame row has been removed.
