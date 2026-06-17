# Extract UIDs from a data list

**\[deprecated\]** Deprecated function for extracting UIDs from a data
list. Please use
[`uids()`](https://branchlab.github.io/metasnf/reference/uids.md)
instead.

## Usage

``` r
get_dl_uids(dl, prefix = FALSE)
```

## Arguments

- dl:

  A nested list of input data from
  [`data_list()`](https://branchlab.github.io/metasnf/reference/data_list.md).

- prefix:

  If TRUE, preserves the "uid\_" prefix added to UIDs when creating a
  data list.

## Value

A character vector of the UID labels contained in a data list.
