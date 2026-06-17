# Extract cluster membership information from a sol_df

**\[deprecated\]** Deprecated function for building extracting cluster
solutions from a solutions data frame. Please use
[`t()`](https://rdrr.io/r/base/t.html) instead.

This function takes in a solutions data frame and returns a data frame
containing the cluster assignments for each uid. It is similar to
'[`get_clusters()`](https://branchlab.github.io/metasnf/reference/get_clusters.md),
which takes one solutions data frame row and returns a vector of cluster
assignments' and
[`get_cluster_df()`](https://branchlab.github.io/metasnf/reference/get_cluster_df.md),
which takes a solutions matrix with only one row and returns a data
frame with two columns: "cluster" and "uid" (the UID of the
observation).

## Usage

``` r
get_cluster_solutions(sol_df)
```

## Arguments

- sol_df:

  A sol_df.

## Value

A "data.frame" object where each row is an observation and each column
(apart from the uid column) indicates the cluster that observation as
assigned to for the corresponding solutions data frame row.
