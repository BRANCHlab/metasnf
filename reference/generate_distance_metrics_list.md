# Generate a list of distance metrics

**\[deprecated\]** Deprecated function for building a distance metrics
list. Please use
[`dist_fns_list()`](https://branchlab.github.io/metasnf/reference/dist_fns_list.md)
(or better yet,
[`snf_config()`](https://branchlab.github.io/metasnf/reference/snf_config.md))
instead.

## Usage

``` r
generate_distance_metrics_list(
  continuous_distances = NULL,
  discrete_distances = NULL,
  ordinal_distances = NULL,
  categorical_distances = NULL,
  mixed_distances = NULL,
  keep_defaults = TRUE
)
```

## Arguments

- continuous_distances:

  A named list of distance metric functions

- discrete_distances:

  A named list of distance metric functions

- ordinal_distances:

  A named list of distance metric functions

- categorical_distances:

  A named list of distance metric functions

- mixed_distances:

  A named list of distance metric functions

- keep_defaults:

  If TRUE (default), prepend the base distance metrics (euclidean and
  standard normalized euclidean)

## Value

A nested and named list of distance metrics functions.
