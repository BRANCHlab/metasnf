# Summarize a data list

**\[deprecated\]** Defunct function for summarizing a data list. Please
use [`summary()`](https://rdrr.io/r/base/summary.html) instead.

## Usage

``` r
summarize_dl(data_list, scope = "component")
```

## Arguments

- data_list:

  A nested list of input data from
  [`data_list()`](https://branchlab.github.io/metasnf/reference/data_list.md).

- scope:

  The level of detail for the summary. Options are:

  - "component" (default): One row per component (data frame) in the
    data list.

  - "feature": One row for each feature in the data list.

## Value

data.frame class object summarizing all components (or features if scope
== "component").
