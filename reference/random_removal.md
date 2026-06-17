# Generate random removal sequence

Helper function to contribute to rows within the settings data frame.
Number of columns removed follows a uniform or exponential probability
distribution.

## Usage

``` r
random_removal(
  columns,
  min_removed_inputs,
  max_removed_inputs,
  dropout_dist = "exponential"
)
```

## Arguments

- columns:

  Columns of the settings_df that are passed in

- min_removed_inputs:

  The smallest number of input data frames that may be randomly removed.

- max_removed_inputs:

  The largest number of input data frames that may be randomly removed.

- dropout_dist:

  Indication of how input data frames should be dropped. can be "none"
  (no dropout), "uniform" (uniformly draw number between min and max
  removed inputs), or "exponential" (like uniform, but using an
  exponential distribution; default).

## Value

Settings data frame row containing inclusion information.
