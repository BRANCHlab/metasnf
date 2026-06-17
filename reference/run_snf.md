# Run SNF

Helper function for running a single SNF config pipeline.

## Usage

``` r
run_snf(i, dl, sc, return_sim_mats, sim_mats_dir, p)
```

## Arguments

- i:

  Row of settings_df and weights_matrix within SNF config to use.

- dl:

  A nested list of input data from
  [`data_list()`](https://branchlab.github.io/metasnf/reference/data_list.md).

- sc:

  An `snf_config` class object which stores all sets of hyperparameters
  used to transform data in dl into a cluster solutions. See
  [`?settings_df`](https://branchlab.github.io/metasnf/reference/settings_df.md)
  or https://branchlab.github.io/metasnf/articles/settings_df.html for
  more details.

- return_sim_mats:

  If TRUE, function will return a list where the first element is the
  solutions data frame and the second element is a list of similarity
  matrices for each row in the sol_df. Default FALSE.

- sim_mats_dir:

  If specified, this directory will be used to save all generated
  similarity matrices.

## Value

A list containing a cluster solution (numeric vector) and a the fused
network used to create that cluster solution. The fused network is NULL
if return_sim_mats is FALSE.
