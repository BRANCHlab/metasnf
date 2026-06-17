# Built-in distance functions

These functions can be used when building a `metasnf` distance functions
list. Each function converts a data frame into to a distance matrix.

## Usage

``` r
euclidean_distance(df, weights_row)

gower_distance(df, weights_row)

sn_euclidean_distance(df, weights_row)

sew_euclidean_distance(df, weights_row)

hamming_distance(df, weights_row)
```

## Arguments

- df:

  Data frame containing at least 1 data column

- weights_row:

  Single-row data frame where the column names contain the column names
  in df and the row contains the corresponding weights_row.

## Value

A matrix class object containing pairwise distances.

## Details

Functions that work for numeric data:

- euclidean_distance: typical Euclidean distance

- sn_euclidean_distance: Data frame is first standardized and normalized
  before typical Euclidean distance is applied

- siw_euclidean_distance: Squared (including weights) Euclidean
  distance, where the weights are also squared

- sew_euclidean_distance: Squared (excluding weights) Euclidean
  distance, where the weights are not also squared

Functions that work for binary data:

- hamming_distance: typical Hamming distance

Functions that work for any type of data:

- gower_distance: Gower distance (cluster::daisy)
