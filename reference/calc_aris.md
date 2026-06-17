# Construct an ARI matrix storing inter-solution similarities

This function constructs an `ari_matrix` class object from a
`solutions_df` class object. The ARI matrix stores pairwise adjusted
Rand indices for all cluster solutions as well as a numeric order for
the solutions data frame based on the hierarchical clustering of the ARI
matrix.

## Usage

``` r
calc_aris(
  sol_df,
  processes = 1,
  verbose = FALSE,
  dist_method = "euclidean",
  hclust_method = "complete"
)
```

## Arguments

- sol_df:

  Solutions data frame containing cluster solutions to calculate
  pairwise ARIs for.

- processes:

  Specify number of processes used to complete calculations

  - `1` (default) Sequential processing

  - `2` or higher: Parallel processing will use the
    [`future.apply::future_apply`](https://future.apply.futureverse.org/reference/future_apply.html)
    to distribute the calculations across the specified number of CPU
    cores. If higher than the number of available cores, a warning will
    be raised and the maximum number of cores will be used.

  - `max`: All available cores will be used. Note that no progress
    indicator is available during multi-core processing.

- verbose:

  If TRUE, output progress to console.

- dist_method:

  Distance method to use when calculating sorting order to of the
  matrix. Argument is directly passed into stats::dist. Options include
  "euclidean", "maximum", "manhattan", "canberra", "binary", or
  "minkowski".

- hclust_method:

  Agglomerative method to use when calculating sorting order by
  [`stats::hclust`](https://rdrr.io/r/stats/hclust.html). Options
  include "ward.D", "ward.D2", "single", "complete", "average",
  "mcquitty", "median", or "centroid".

## Value

om_aris ARIs between clustering solutions of an solutions data frame

## Examples

``` r
dl <- data_list(
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)
#> ℹ 168 observations dropped due to incomplete data.

sc <- snf_config(dl, n_solutions = 3)
#> ℹ No distance functions specified. Using defaults.
#> ℹ No clustering functions specified. Using defaults.
sol_df <- batch_snf(dl, sc)
calc_aris(sol_df)
#> ARI matrix for 3 cluster solutions.
#>           1         2         3
#> 1 1.0000000 0.9834037 0.9351973
#> 2 0.9834037 1.0000000 0.9179166
#> 3 0.9351973 0.9179166 1.0000000
#> ARI-based order: 3 1 2 
```
