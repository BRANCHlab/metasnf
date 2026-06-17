# Launch a shiny app to identify meta cluster boundaries

This function calls the `htShiny()` function from the package
InteractiveComplexHeatmap to assist users in identifying the indices of
the boundaries between meta clusters in a meta cluster heatmap. By
providing a heatmap of inter-solution similarities (obtained through
meta_cluster_heatmap()), users can click on positions within the heatmap
that appear to meaningfully separate major sets of similar cluster
solutions by visual inspection. The corresponding indices of the clicked
positions are printed to the console and also shown within the app. This
function can only run from an interactive session of R.

## Usage

``` r
shiny_annotator(ari_heatmap)
```

## Arguments

- ari_heatmap:

  Heatmap of ARIs to divide into meta clusters.

## Value

Does not return any value. Launches interactive shiny applet.

## Examples

``` r
# \donttest{
dl <- data_list(
    list(cort_sa, "cortical_surface_area", "neuroimaging", "continuous"),
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)
#> ℹ 175 observations dropped due to incomplete data.

set.seed(42)
my_sc <- snf_config(
    dl = dl,
    n_solutions = 20,
    min_k = 20,
    max_k = 50
)
#> ℹ No distance functions specified. Using defaults.
#> ℹ No clustering functions specified. Using defaults.

sol_df <- batch_snf(dl, my_sc)

sol_aris <- calc_aris(sol_df)

meta_cluster_order <- get_matrix_order(sol_aris)

ari_hm <- meta_cluster_heatmap(sol_aris, order = meta_cluster_order)
#> Sorting by order.

# Click on meta cluster boundaries to obtain `split_vec` values
shiny_annotator(ari_hm)
#> ℹ Ignoring `shiny_annotator()` call in non-interactive session.

split_vec <- c(6, 10, 16)

ari_hm <- meta_cluster_heatmap(
    sol_aris,
    order = meta_cluster_order,
    split_vector = split_vec
)
#> Sorting by order.
# }
```
