# Extract representative solutions from a matrix of ARIs

Following clustering with `batch_snf`, a matrix of pairwise ARIs that
show how related each cluster solution is to each other can be generated
by the `calc_aris` function. Partitioning of the ARI matrix can be done
by visual inspection of
[`meta_cluster_heatmap()`](https://branchlab.github.io/metasnf/reference/plot.ari_matrix.md)
results or by `shiny_annotator`. Given the indices of meta cluster
boundaries, this function will return a single representative solution
from each meta cluster based on maximum average ARI to all other
solutions within that meta cluster.

## Usage

``` r
get_representative_solutions(aris, sol_df, filter_fn = NULL)
```

## Arguments

- aris:

  Matrix of adjusted rand indices from
  [`calc_aris()`](https://branchlab.github.io/metasnf/reference/calc_aris.md)

- sol_df:

  Output of `batch_snf` containing cluster solutions.

- filter_fn:

  Optional function to filter the meta-cluster by prior to maximum
  average ARI determination. This can be useful if you are explicitly
  trying to select a solution that meets a certain condition, such as
  only picking from the 4 cluster solutions within a meta cluster. An
  example valid function could be
  `fn <- function(x) x[x$"nclust" == 4, ]`.

## Value

The provided solutions data frame reduced to just one row per meta
cluster defined by the split vector.

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
    
    # Identify meta cluster boundaries with shiny app or trial and error
    # ari_hm <- meta_cluster_heatmap(sol_aris, order = meta_cluster_order)
    # shiny_annotator(ari_hm)
    
    # Result of meta cluster examination
    split_vec <- c(2, 5, 12, 17)
    
    ext_sol_df <- label_meta_clusters(ext_sol_df, split_vec, meta_cluster_order)
    
    # Extracting representative solutions from each defined meta cluster
    rep_solutions <- get_representative_solutions(sol_aris, ext_sol_df)
# }
```
