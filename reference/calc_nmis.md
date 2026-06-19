# Calculate feature NMIs for a data list and a solutions data frame

Normalized mutual information scores can be used to indirectly measure
how important a feature may have been in producing a cluster solution.
This function will calculate the normalized mutual information between
cluster solutions in a solutions data frame as well as cluster solutions
created by including only a single feature from a provided data list,
but otherwise using all the same hyperparameters as specified in the
original SNF config. Note that NMIs can be calculated between two
cluster solutions regardless of what features were actually used to
create those cluster solutions. For example, a feature that was not
involved in producing a particular cluster solution may still have a
high NMI with that cluster solution (typically because it was highly
correlated with a different feature that was used).

## Usage

``` r
calc_nmis(
  dl,
  sol_df,
  transpose = TRUE,
  ignore_inclusions = TRUE,
  processes = 1,
  verbose = FALSE
)
```

## Arguments

- dl:

  A nested list of input data from
  [`data_list()`](https://branchlab.github.io/metasnf/reference/data_list.md).

- sol_df:

  Result of `batch_snf` storing cluster solutions and the settings that
  were used to generate them. Use the same value as was used in the
  original call to
  [`batch_snf()`](https://branchlab.github.io/metasnf/reference/batch_snf.md).

- transpose:

  If TRUE, will transpose the output data frame.

- ignore_inclusions:

  If TRUE, will ignore the inclusion columns in the solutions data frame
  and calculate NMIs for all features. If FALSE, will give NAs for
  features that were dropped on a given settings_df row.

- processes:

  Specify number of processes used to complete SNF iterations

  - `1` (default) Sequential processing: function will iterate through
    the `settings_df` one row at a time with a for loop. This option
    will not make use of multiple CPU cores, but will show a progress
    bar.

  - `2` or higher: Parallel processing will use the
    [`future.apply::future_apply`](https://future.apply.futureverse.org/reference/future_apply.html)
    to distribute the SNF iterations across the specified number of CPU
    cores. If higher than the number of available cores, a warning will
    be raised and the maximum number of cores will be used.

  - `max`: All available cores will be used.

- verbose:

  If TRUE, output progress to console.

## Value

A "data.frame" class object containing one row for every feature in the
provided data list and one column for every solution in the provided
solutions data frame. Populated values show the calculated NMI score for
each feature-solution combination.

## Examples

``` r
input_dl <- data_list(
    list(gender_df, "gender", "demographics", "categorical"),
    list(diagnosis_df, "diagnosis", "clinical", "categorical"),
    uid = "patient_id"
)

sc <- snf_config(input_dl, n_solutions = 2)
#> ℹ No distance functions specified. Using defaults.
#> ℹ No clustering functions specified. Using defaults.

sol_df <- batch_snf(input_dl, sc)

calc_nmis(input_dl, sol_df)
#>     feature        s1        s2
#> 1    gender 0.2684083 0.2120130
#> 2 diagnosis 0.9035042 0.9240358
```
