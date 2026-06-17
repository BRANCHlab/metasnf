# Heatmap of pairwise adjusted rand indices between solutions

**\[deprecated\]** Defunct function to create an ARI heatmap. Please use
[`meta_cluster_heatmap()`](https://branchlab.github.io/metasnf/reference/plot.ari_matrix.md)
instead.

## Usage

``` r
adjusted_rand_index_heatmap(
  aris,
  order = NULL,
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  log_graph = FALSE,
  scale_diag = "none",
  min_colour = "#282828",
  max_colour = "firebrick2",
  col = circlize::colorRamp2(c(min(aris), max(aris)), c(min_colour, max_colour)),
  ...
)
```

## Arguments

- aris:

  Matrix of adjusted rand indices from
  [`calc_aris()`](https://branchlab.github.io/metasnf/reference/calc_aris.md)

- order:

  Numeric vector containing row order of the heatmap.

- cluster_rows:

  Whether rows should be clustered.

- cluster_columns:

  Whether columns should be clustered.

- log_graph:

  If TRUE, log transforms the graph.

- scale_diag:

  Method of rescaling matrix diagonals. Can be "none" (don't change
  diagonals), "mean" (replace diagonals with average value of
  off-diagonals), or "zero" (replace diagonals with 0).

- min_colour:

  Colour used for the lowest value in the heatmap.

- max_colour:

  Colour used for the highest value in the heatmap.

- col:

  Colour ramp to use for the heatmap.

- ...:

  Additional parameters passed to
  [`similarity_matrix_heatmap()`](https://branchlab.github.io/metasnf/reference/similarity_matrix_heatmap.md),
  the function that this function wraps.

## Value

Returns a heatmap (class "Heatmap" from package ComplexHeatmap) that
displays the pairwise adjusted Rand indices (similarities) between the
cluster solutions of the provided solutions data frame.
