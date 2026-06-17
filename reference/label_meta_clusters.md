# Assign meta cluster labels to rows of a solutions data frame or extended solutions data frame

Given a solutions data frame or extended solutions data frame class
object and a numeric vector indicating which rows correspond to which
meta clusters, assigns meta clustering information to the
"meta_clusters" attribute of the data frame.

## Usage

``` r
label_meta_clusters(sol_df, split_vector, order = NULL)
```

## Arguments

- sol_df:

  A solutions data frame or extended solutions data frame to assign meta
  clusters to.

- split_vector:

  A numeric vector indicating which rows of sol_df should be the split
  points for meta cluster labeling.

- order:

  An optional numeric vector indicating how the solutions data frame
  should be reordered prior to meta cluster labeling. This vector can be
  obtained by running
  [`get_matrix_order()`](https://branchlab.github.io/metasnf/reference/get_matrix_order.md)
  on an ARI matrix, which itself can be obtained by calling
  [`calc_aris()`](https://branchlab.github.io/metasnf/reference/calc_aris.md)
  on a solutions data frame.

## Value

A solutions data frame with a populated "meta_clusters" attribute.

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
    
    sol_df
#> 20 cluster solutions of 100 observations:
#> solution nclust mc uid_NDAR_INV0567T2Y9 uid_NDAR_INV0IZ157F8 
#> 8;5;250m 1     1      8 NA                       5                    2 
#> 8;5;250m 2     2      4 NA                       1                    3 
#> 8;5;250m 3     3      2 NA                       2                    1 
#> 8;5;250m 4     4      2 NA                       2                    1 
#> 8;5;250m 5     5      3 NA                       3                    1 
#> 8;5;250m 6     6      2 NA                       2                    1 
#> 8;5;250m 7     7      4 NA                       1                    2 
#> 8;5;250m 8     8      6 NA                       5                    4 
#> 8;5;250m 9     9      2 NA                       1                    2 
#> 8;5;250m 0    10      2 NA                       2                    1 
#> 8;5;246m  ℹ 10  or ws 
#> 8;5;246m  ℹ 98  or riables: uid_NDAR_INV0J4PYA5F <dbl>, uid_NDAR_INV10OMKVLE <dbl>, 
#> 8;5;246m    uid ND NV15FPCW4O <dbl>, uid_NDAR_INV19NB4RJK <dbl>, 
#> 8;5;246m    uid ND NV1HLGR738 <dbl>, uid_NDAR_INV1KR0EZFU <dbl>, 
#> 8;5;246m    uid ND NV1L3Y9EOP <dbl>, uid_NDAR_INV1TCP5GNM <dbl>, 
#> 8;5;246m    uid ND NV1ZHRDJ6B <dbl>, uid_NDAR_INV2EJ41YSZ <dbl>, 
#> 8;5;246m    uid ND NV2PK6C85M <dbl>, uid_NDAR_INV2XO1PHCT <dbl>, … 
#> 3 solutions and 98 observations not shown.
#> Use `print(n = ...)` to change the number of rows printed.
#> Use `t()` to view compact cluster solution format.
#> 
    
    sol_aris <- calc_aris(sol_df)
    
    meta_cluster_order <- get_matrix_order(sol_aris)
    
    # `split_vec` found by iteratively plotting ari_hm or by ?shiny_annotator()
    split_vec <- c(6, 10, 16)
    ari_hm <- meta_cluster_heatmap(
        sol_aris,
        order = meta_cluster_order,
        split_vector = split_vec
    )
#> Sorting by order.
    
    mc_sol_df <- label_meta_clusters(
        sol_df,
        order = meta_cluster_order,
        split_vector = split_vec
    )
    
    mc_sol_df
#> 20 cluster solutions of 100 observations:
#> solution nclust mc uid_NDAR_INV0567T2Y9 uid_NDAR_INV0IZ157F8 
#> 8;5;250m 1     1      8 C                        5                    2 
#> 8;5;250m 2     2      4 C                        1                    3 
#> 8;5;250m 3     3      2 A                        2                    1 
#> 8;5;250m 4     4      2 A                        2                    1 
#> 8;5;250m 5     5      3 B                        3                    1 
#> 8;5;250m 6     6      2 A                        2                    1 
#> 8;5;250m 7     7      4 D                        1                    2 
#> 8;5;250m 8     8      6 C                        5                    4 
#> 8;5;250m 9     9      2 B                        1                    2 
#> 8;5;250m 0    10      2 A                        2                    1 
#> 8;5;246m  ℹ 10  or ws 
#> 8;5;246m  ℹ 98  or riables: uid_NDAR_INV0J4PYA5F <dbl>, uid_NDAR_INV10OMKVLE <dbl>, 
#> 8;5;246m    uid ND NV15FPCW4O <dbl>, uid_NDAR_INV19NB4RJK <dbl>, 
#> 8;5;246m    uid ND NV1HLGR738 <dbl>, uid_NDAR_INV1KR0EZFU <dbl>, 
#> 8;5;246m    uid ND NV1L3Y9EOP <dbl>, uid_NDAR_INV1TCP5GNM <dbl>, 
#> 8;5;246m    uid ND NV1ZHRDJ6B <dbl>, uid_NDAR_INV2EJ41YSZ <dbl>, 
#> 8;5;246m    uid ND NV2PK6C85M <dbl>, uid_NDAR_INV2XO1PHCT <dbl>, … 
#> 3 solutions and 98 observations not shown.
#> Use `print(n = ...)` to change the number of rows printed.
#> Use `t()` to view compact cluster solution format.
#> 
# }
```
