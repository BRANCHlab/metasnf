# Create subsamples of a data list

Given a data list, return a list of smaller data lists that are
generated through random sampling (without replacement). The results of
this function can be passed into
[`batch_snf_subsamples()`](https://branchlab.github.io/metasnf/reference/batch_snf_subsamples.md)
to obtain a list of resampled solutions data frames.

## Usage

``` r
subsample_dl(
  dl,
  n_subsamples,
  subsample_fraction = NULL,
  n_observations = NULL
)
```

## Arguments

- dl:

  A nested list of input data from
  [`data_list()`](https://branchlab.github.io/metasnf/reference/data_list.md).

- n_subsamples:

  Number of subsamples to create.

- subsample_fraction:

  Percentage of patients to include per subsample.

- n_observations:

  Number of patients to include per subsample.

## Value

A "list" class object containing `n_subsamples` number of data lists.
Each of those data lists contains a random `subsample_fraction` fraction
of the observations of the provided data list.

## Examples

``` r
my_dl <- data_list(
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)
#> ℹ 175 observations dropped due to incomplete data.

my_dl_subsamples <- subsample_dl(
    my_dl,
    n_subsamples = 20,
    subsample_fraction = 0.85
)
```
