# Extend a solutions data frame to include outcome evaluations

Extend a solutions data frame to include outcome evaluations

## Usage

``` r
extend_solutions(
  sol_df,
  target_dl = NULL,
  dl = NULL,
  cat_test = "chi_squared",
  min_pval = 1e-10,
  processes = 1,
  verbose = FALSE
)
```

## Arguments

- sol_df:

  Result of `batch_snf` storing cluster solutions and the settings that
  were used to generate them.

- target_dl:

  A data list with features to calculate p-values for. Features in the
  target list will be included during p-value summary measure
  calculations.

- dl:

  A data list with features to calculate p-values for, but that should
  not be incorporated into p-value summary measure columns (i.e.,
  min/mean/max p-value columns).

- cat_test:

  String indicating which statistical test will be used to associate
  cluster with a categorical feature. Options are "chi_squared" for the
  Chi-squared test and "fisher_exact" for Fisher's exact test.

- min_pval:

  If assigned a value, any p-value less than this will be replaced with
  this value.

- processes:

  The number of processes to use for parallelization. Progress is only
  reported for sequential processing (processes = 1).

- verbose:

  If TRUE, output progress to console.

## Value

An extended solutions data frame (`ext_sol_df` class object) that
contains p-value columns for each outcome in the provided data lists

## Examples

``` r
if (FALSE) { # \dontrun{
    input_dl <- data_list(
        list(gender_df, "gender", "demographics", "categorical"),
        list(diagnosis_df, "diagnosis", "clinical", "categorical"),
        uid = "patient_id"
    )
    
    sc <- snf_config(input_dl, n_solutions = 2)
    
    sol_df <- batch_snf(input_dl, sc)
    
    ext_sol_df <- extend_solutions(sol_df, input_dl)
} # }
```
