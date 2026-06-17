# Jitter plot separating a feature by cluster

Jitter plot separating a feature by cluster

## Usage

``` r
jitter_plot(df, feature)
```

## Arguments

- df:

  A data.frame containing cluster column and the feature to plot.

- feature:

  The feature to plot.

## Value

A jitter+violin plot (class "gg", "ggplot") showing the distribution of
a feature across clusters.
