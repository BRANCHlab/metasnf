# SNF schemes

These functions manage the way in which input data frames are passed
into SNF to yield a final fused network.

## Usage

``` r
two_step_merge(
  dl,
  k = 20,
  alpha = 0.5,
  t = 20,
  cnt_dist_fn,
  dsc_dist_fn,
  ord_dist_fn,
  cat_dist_fn,
  mix_dist_fn,
  weights_row
)

domain_merge(
  dl,
  cnt_dist_fn,
  dsc_dist_fn,
  ord_dist_fn,
  cat_dist_fn,
  mix_dist_fn,
  weights_row,
  k,
  alpha,
  t
)

individual(
  dl,
  k = 20,
  alpha = 0.5,
  t = 20,
  cnt_dist_fn,
  dsc_dist_fn,
  ord_dist_fn,
  cat_dist_fn,
  mix_dist_fn,
  weights_row
)
```

## Arguments

- dl:

  A nested list of input data from
  [`data_list()`](https://branchlab.github.io/metasnf/reference/data_list.md).

- k:

  k hyperparameter.

- alpha:

  alpha/eta/sigma hyperparameter.

- t:

  SNF number of iterations hyperparameter.

- cnt_dist_fn:

  distance metric function for continuous data.

- dsc_dist_fn:

  distance metric function for discrete data.

- ord_dist_fn:

  distance metric function for ordinal data.

- cat_dist_fn:

  distance metric function for categorical data.

- mix_dist_fn:

  distance metric function for mixed data.

- weights_row:

  data frame row containing feature weights.

## Details

individual: The "vanilla" scheme - does distance matrix conversions of
each input data frame separately before a single call to SNF fuses them
into the final fused network.

domain_merge: Given a data list, returns a new data list where all data
objects of a particular domain have been concatenated.

two_step_merge: Individual data frames into individual similarity
matrices into one fused network per domain into one final fused network.
