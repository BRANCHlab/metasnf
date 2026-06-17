# Helper function for using the correct SNF scheme

Helper function for using the correct SNF scheme

## Usage

``` r
snf_step(
  dl,
  scheme,
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

- scheme:

  Which SNF system to use to achieve the final fused network.

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

## Value

A fused similarity network (matrix).
