# Estimate number of clusters for a similarity matrix

Calculate eigengap and rotation-cost estimates of the number of clusters
to use when clustering a similarity matrix. This function was adapted
from `SNFtool::estimateClustersGivenGraph`, but scales up the Laplacian
operator prior to eigenvalue calculations to minimize the risk of
floating point-related errors.

## Usage

``` r
estimate_nclust_given_graph(W, NUMC = 2:10)
```

## Arguments

- W:

  Similarity matrix to calculate number of clusters for.

- NUMC:

  Range of cluster counts to consider among when picking best number of
  clusters.

## Value

A list containing the top two eigengap and rotation-cost estimates for
the number of clusters in a given similarity matrix.

## Examples

``` r
input_dl <- data_list(
    list(gender_df, "gender", "demographics", "categorical"),
    list(diagnosis_df, "diagnosis", "clinical", "categorical"),
    uid = "patient_id"
)

sc <- snf_config(input_dl, n_solutions = 1)
#> ℹ No distance functions specified. Using defaults.
#> ℹ No clustering functions specified. Using defaults.
sol_df <- batch_snf(input_dl, sc, return_sim_mats = TRUE)
sim_mat <- sim_mats_list(sol_df)[[1]]
estimate_nclust_given_graph(sim_mat)
#> $`Eigen-gap best`
#> [1] 4
#> 
#> $`Eigen-gap 2nd best`
#> [1] 3
#> 
#> $`Rotation cost best`
#> [1] 4
#> 
#> $`Rotation cost 2nd best`
#> [1] 3
#> 
```
