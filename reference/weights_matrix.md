# Generate a matrix to store feature weights

Function for building a weights matrix independently of an SNF config.
The weights matrix contains one row corresponding to each row of the
settings data frame in an SNF config (one row for each resulting cluster
solution) and one column for each feature in the data list used for
clustering. Values of the weights matrix are passed to distance metrics
functions during the conversion of input data frames to distance
matrices. Typically, there is no need to use this function directly.
Instead, users should provide weights matrix-building parameters to the
[`snf_config()`](https://branchlab.github.io/metasnf/reference/snf_config.md)
function.

## Usage

``` r
weights_matrix(dl = NULL, n_solutions = 1, weights_fill = "ones")
```

## Arguments

- dl:

  A nested list of input data from
  [`data_list()`](https://branchlab.github.io/metasnf/reference/data_list.md).

- n_solutions:

  Number of rows to generate the template weights matrix for.

- weights_fill:

  String indicating what to populate generate rows with. Can be "ones"
  (default; fill matrix with 1), "uniform" (fill matrix with uniformly
  distributed random values), or "exponential" (fill matrix with
  exponentially distributed random values).

## Value

wm A properly formatted matrix containing columns for all the features
that require weights and rows.

## Examples

``` r
input_dl <- data_list(
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)
#> ℹ 175 observations dropped due to incomplete data.

sc <- snf_config(input_dl, n_solutions = 5)
#> ℹ No distance functions specified. Using defaults.
#> ℹ No clustering functions specified. Using defaults.

wm <- weights_matrix(input_dl, n_solutions = 5, weights_fill = "uniform")

# updating an SNF config in parts
sc$"weights_matrix" <- wm
```
