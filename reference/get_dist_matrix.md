# Calculate distance matrices

Given a data frame of numerical features, return a euclidean distance
matrix.

## Usage

``` r
get_dist_matrix(
  df,
  input_type,
  cnt_dist_fn,
  dsc_dist_fn,
  ord_dist_fn,
  cat_dist_fn,
  mix_dist_fn,
  weights_row
)
```

## Arguments

- df:

  Raw data frame with subject IDs in column "uid"

- input_type:

  Either "numeric" (resulting in euclidean distances), "categorical"
  (resulting in binary distances), or "mixed" (resulting in gower
  distances)

- cnt_dist_fn:

  distance metric function for continuous data

- dsc_dist_fn:

  distance metric function for discrete data

- ord_dist_fn:

  distance metric function for ordinal data

- cat_dist_fn:

  distance metric function for categorical data

- mix_dist_fn:

  distance metric function for mixed data

- weights_row:

  Single-row data frame where the column names contain the column names
  in df and the row contains the corresponding weights_row.

## Value

dist_matrix Matrix of inter-observation distances.
