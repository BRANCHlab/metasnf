# Extract cluster membership information from one solutions data frame row

**\[deprecated\]** Deprecated function for building extracting cluster
solutions from a solutions data frame. Please use
[`t()`](https://rdrr.io/r/base/t.html) instead.

This function takes in a single row of a solutions data frame and
returns a data frame containing the cluster assignments for each uid. It
is similar to
[`get_clusters()`](https://branchlab.github.io/metasnf/reference/get_clusters.md),
which takes one solutions data frame row and returns a vector of cluster
assignments' and
[`get_cluster_solutions()`](https://branchlab.github.io/metasnf/reference/get_cluster_solutions.md),
which takes a solutions data frame with any number of rows and returns a
data frame indicating the cluster assignments for each of those rows.

## Usage

``` r
get_cluster_df(sol_df_row)
```

## Arguments

- sol_df_row:

  One row from a solutions data frame.

## Value

cluster_df data frame of cluster and uid.
