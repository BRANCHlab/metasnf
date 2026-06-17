# Build a distance metrics list

The distance metrics list object (inherits classes `dist_fns_list` and
`list`) is a list that stores R functions which can convert a data frame
of features into a matrix of pairwise distances. The list is a nested
one, where the first layer of the list can hold up to 5 items (one for
each of the `metasnf` recognized feature types, continuous, discrete,
ordinal, categorical, and mixed), and the second layer can hold an
arbitrary number of distance functions for each of those types.

## Usage

``` r
dist_fns_list(
  cnt_dist_fns = NULL,
  dsc_dist_fns = NULL,
  ord_dist_fns = NULL,
  cat_dist_fns = NULL,
  mix_dist_fns = NULL,
  automatic_standard_normalize = FALSE,
  use_default_dist_fns = FALSE
)
```

## Arguments

- cnt_dist_fns:

  A named list of continuous distance metric functions.

- dsc_dist_fns:

  A named list of discrete distance metric functions.

- ord_dist_fns:

  A named list of ordinal distance metric functions.

- cat_dist_fns:

  A named list of categorical distance metric functions.

- mix_dist_fns:

  A named list of mixed distance metric functions.

- automatic_standard_normalize:

  If TRUE, will automatically use standard normalization prior to
  calculation of any numeric distances. This parameter overrides all
  other distance functions list-related parameters.

- use_default_dist_fns:

  If TRUE, prepend the base distance metrics (euclidean distance for
  continuous, discrete, and ordinal data and gower distance for
  categorical and mixed data) to the resulting distance metrics list.

## Value

A distance metrics list object.

## Details

Call ?dist_fns to see all distance metric functions provided in metasnf.

## Examples

``` r
# Using just the base distance metrics  ------------------------------------
dist_fns_list <- dist_fns_list()
#> ℹ No distance functions specified. Using defaults.

# Adding your own metrics --------------------------------------------------
# This will contain only the and user-provided distance function:
cubed_euclidean <- function(df, weights_row) {
    # (your code that converts a data frame to a distance metric here...)
    weights <- diag(weights_row, nrow = length(weights_row))
    weighted_df <- as.matrix(df) %*% weights
    distance_matrix <- weighted_df |>
        stats::dist(method = "euclidean") |>
        as.matrix()
    distance_matrix <- distance_matrix^3
    return(distance_matrix)
}

dist_fns_list <- dist_fns_list(
    cnt_dist_fns = list(
         "my_cubed_euclidean" = cubed_euclidean
    )
)

# Using default base metrics------------------------------------------------
# Call ?dist_fns to see all distance metric functions provided in
# metasnf. The code below will contain a mix of user-provided and built-in
# distance metric functions.
dist_fns_list <- dist_fns_list(
    cnt_dist_fns = list(
         "my_distance_metric" = cubed_euclidean
    ),
    dsc_dist_fns = list(
         "my_distance_metric" = cubed_euclidean
    ),
    ord_dist_fns = list(
         "my_distance_metric" = cubed_euclidean
    ),
    cat_dist_fns = list(
         "my_distance_metric" = gower_distance
    ),
    mix_dist_fns = list(
         "my_distance_metric" = gower_distance
    ),
    use_default_dist_fns = TRUE
)
```
