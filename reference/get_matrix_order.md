# Return the hierarchical clustering order of a matrix

Return the hierarchical clustering order of a matrix

## Usage

``` r
get_matrix_order(matrix, dist_method = "euclidean", hclust_method = "complete")
```

## Arguments

- matrix:

  Matrix to cluster.

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

A numeric vector of the ordering derived by the specified hierarchical
clustering method applied to the provided matrix.

## Examples

``` r
# \donttest{
dl <- data_list(
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    list(depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)
#> ℹ 188 observations dropped due to incomplete data.

sc <- snf_config(
    dl = dl,
    n_solutions = 20,
    min_k = 20,
    max_k = 50
)
#> ℹ No distance functions specified. Using defaults.
#> ℹ No clustering functions specified. Using defaults.

sol_df <- batch_snf(dl, sc)

ext_sol_df <- extend_solutions(
    sol_df,
    dl = dl,
    min_pval = 1e-10 # p-values below 1e-10 will be thresholded to 1e-10
)

# Calculate pairwise similarities between cluster solutions
sol_aris <- calc_aris(sol_df)

# Extract hierarchical clustering order of the cluster solutions
meta_cluster_order <- get_matrix_order(sol_aris)
# }
```
