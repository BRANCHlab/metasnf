# Plot of feature values in a data list

This plot, built on
[`ComplexHeatmap::Heatmap()`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html),
visualizes the feature values in a data list as a continuous heatmap
with observations along the columns and features along the rows.

## Usage

``` r
# S3 method for class 'data_list'
plot(
  x,
  y = NULL,
  cluster_rows = TRUE,
  cluster_columns = TRUE,
  heatmap_legend_param = NULL,
  row_title = "Observation",
  column_title = "Feature",
  show_row_names = FALSE,
  ...
)
```

## Arguments

- x:

  A `data_list` object.

- y:

  Optional argument to `plot`, not used in this method.

- cluster_rows:

  Logical indicating whether to cluster the rows (observations).

- cluster_columns:

  Logical indicating whether to cluster the columns (features).

- heatmap_legend_param:

  A list of parameters for the heatmap legend.

- row_title:

  Title for the rows (observations).

- column_title:

  Title for the columns (features).

- show_row_names:

  Logical indicating whether to show row names.

- ...:

  Additional arguments passed to
  [`ComplexHeatmap::Heatmap()`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html).

## Value

A heatmap visualization of feature values.
