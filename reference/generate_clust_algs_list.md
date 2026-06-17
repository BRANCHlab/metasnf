# Generate a clustering algorithms list

**\[deprecated\]** Deprecated function for building a clustering
algorithms list. Please use
[`clust_fns_list()`](https://branchlab.github.io/metasnf/reference/clust_fns_list.md)
(or better yet,
[`snf_config()`](https://branchlab.github.io/metasnf/reference/snf_config.md))
instead.

## Usage

``` r
generate_clust_algs_list(..., disable_base = FALSE)
```

## Arguments

- ...:

  An arbitrary number of named clustering functions

- disable_base:

  If TRUE, do not prepend the base clustering algorithms (spectral_eigen
  and spectral_rot, which apply spectral clustering and use the
  eigen-gap and rotation cost heuristics respectively for determining
  the number of clusters in the graph.

## Value

A list of clustering algorithm functions that can be passed into the
batch_snf and generate_settings_list functions.
