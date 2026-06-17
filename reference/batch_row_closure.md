# Generate closure function to run batch_snf in an apply-friendly format

Generate closure function to run batch_snf in an apply-friendly format

## Usage

``` r
batch_row_closure(
  dl,
  dfl,
  cfl,
  sdf,
  wm,
  similarity_matrix_dir,
  return_sim_mats,
  prog
)
```

## Arguments

- dl:

  A nested list of input data from
  [`data_list()`](https://branchlab.github.io/metasnf/reference/data_list.md).

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

- prog:

  Progressr function to update parallel processing progress

## Value

A "function" class object used to run `batch_snf` in lapply-form for
parallel processing.
