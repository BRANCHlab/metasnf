# Return the row or column ordering present in a heatmap

Return the row or column ordering present in a heatmap

## Usage

``` r
get_heatmap_order(heatmap, type = "rows")
```

## Arguments

- heatmap:

  A heatmap object to collect ordering from.

- type:

  The type of ordering to return. Either "rows" or "columns".

## Value

A numeric vector of the ordering used within the provided ComplexHeatmap
"Heatmap" object.
