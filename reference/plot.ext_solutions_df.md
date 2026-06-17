# Plot of cluster assignments in an extended solutions data frame

This plot, built on
[`ComplexHeatmap::Heatmap()`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html),
visualizes the cluster assignments in a solutions data frame as a
categorical heatmap with observations along the columns and clusters
along the rows.

## Usage

``` r
# S3 method for class 'ext_solutions_df'
plot(
  x,
  y = NULL,
  cluster_rows = TRUE,
  cluster_columns = TRUE,
  show_row_names = TRUE,
  show_column_names = TRUE,
  heatmap_legend_param = NULL,
  row_title = "Solution",
  column_title = "Observation",
  ...
)

# S3 method for class 't_ext_solutions_df'
plot(x, ...)
```

## Arguments

- x:

  An `ext_solutions_df` object.

- y:

  Optional argument to `plot`, not used in this method.

- cluster_rows:

  If the value is a logical, it controls whether to make cluster on
  rows. The value can also be a
  [`hclust`](https://rdrr.io/r/stats/hclust.html) or a
  [`dendrogram`](https://rdrr.io/r/stats/dendrogram.html) which already
  contains clustering. Check
  <https://jokergoo.github.io/ComplexHeatmap-reference/book/a-single-heatmap.html#clustering>
  .

- cluster_columns:

  Whether make cluster on columns? Same settings as `cluster_rows`.

- show_row_names:

  Whether show row names.

- show_column_names:

  Whether show column names.

- heatmap_legend_param:

  A list contains parameters for the heatmap legends. See
  [`color_mapping_legend,ColorMapping-method`](https://rdrr.io/pkg/ComplexHeatmap/man/color_mapping_legend-ColorMapping-method.html)
  for all available parameters.

- row_title:

  Title on the row.

- column_title:

  Title on the column.

- ...:

  Additional arguments passed to
  [`ComplexHeatmap::Heatmap()`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html).

## Value

A
[`ComplexHeatmap::Heatmap()`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html)
object visualization of cluster assignments.
