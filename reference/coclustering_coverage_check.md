# Co-clustering coverage check

Check if co-clustered data has at least one subsample in which every
pair of observations were a part of simultaneously.

## Usage

``` r
coclustering_coverage_check(cocluster_df, action = "warn")
```

## Arguments

- cocluster_df:

  data frame containing co-clustering data.

- action:

  Control if parent function should warn or stop.

## Value

This function does not return any value. It checks a `cocluster_df` for
complete coverage (all pairs occur in the same solution at least once).
Will raise a warning or error if coverage is incomplete depending on the
value of the action parameter.
