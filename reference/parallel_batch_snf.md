# Parallel processing form of batch_snf

Parallel processing form of batch_snf

## Usage

``` r
parallel_batch_snf(
  dl,
  dfl,
  cfl,
  sdf,
  wm,
  similarity_matrix_dir,
  return_sim_mats,
  processes
)
```

## Arguments

- dl:

  A data list.

- dfl:

  An optional nested list containing which distance metric function
  should be used for the various feature types (continuous, discrete,
  ordinal, categorical, and mixed). See ?dist_fns_list for details on
  how to build this.

- cfl:

  List of custom clustering algorithms to apply to the final fused
  network. See ?clust_fns_list.

- sdf:

  matrix indicating parameters to iterate SNF through.

- wm:

  A matrix containing feature weights to use during distance matrix
  calculation. See ?weights_matrix for details on how to build this.

- similarity_matrix_dir:

  If specified, this directory will be used to save all generated
  similarity matrices.

- return_sim_mats:

  If TRUE, function will return a list where the first element is the
  solutions data frame and the second element is a list of similarity
  matrices for each row in the sol_df. Default FALSE.

- processes:

  Number of parallel processes used when executing SNF.

## Value

The same values as ?batch_snf().
