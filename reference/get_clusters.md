# Extract cluster membership vector from one solutions data frame row

**\[deprecated\]** Deprecated function for building extracting cluster
solutions from a solutions data frame. Please use
[`t()`](https://rdrr.io/r/base/t.html) instead.

This function takes in a single row of a solutions data frame and
returns a vector containing the cluster assignments for each
observation. It is similar to
[`get_cluster_df()`](https://branchlab.github.io/metasnf/reference/get_cluster_df.md),
which takes a solutions data frame with only one row and returns a data
frame with two columns: "cluster" and "uid" '(the UID of the
observation) and
[`get_cluster_solutions()`](https://branchlab.github.io/metasnf/reference/get_cluster_solutions.md),
which takes a solutions data frame with any number of rows and returns a
data frame indicating the cluster assignments for each of those rows.

## Usage

``` r
get_clusters(sol_df_row)
```

## Arguments

- sol_df_row:

  Output matrix row.

## Value

clusters Vector of assigned clusters.
