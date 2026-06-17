# Place significance stars on ComplexHeatmap cells

This is an internal function meant to be used to by the
assoc_pval_heatmap function.

## Usage

``` r
cell_significance_fn(data)
```

## Arguments

- data:

  The matrix containing the cells to base the significance stars on.

## Value

cell_fn Another function that is well-formatted for usage as the
cell_fun argument in ComplexHeatmap::Heatmap.
