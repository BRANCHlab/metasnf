# Variable-level summary of a data list

**\[deprecated\]** Defunct function to summarize a data list. Please use
[`summary()`](https://rdrr.io/r/base/summary.html) with argument
`scope = "feature"` instead.

## Usage

``` r
dl_variable_summary(dl)
```

## Arguments

- dl:

  A nested list of input data from
  [`data_list()`](https://branchlab.github.io/metasnf/reference/data_list.md).

## Value

variable_level_summary A data frame containing the name, type, and
domain of every variable in a data list.
