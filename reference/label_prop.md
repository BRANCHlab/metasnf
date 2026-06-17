# Label propagation

Given a full fused network (one containing both pre-clustered
observations and to-be-clustered observations) and the clusters of the
pre-clustered observations, return a label propagated list of clusters
for all observations. This function is derived from
SNFtool::groupPredict. Modifications are made to take a full fused
network as input, rather than taking input data frames and running SNF
internally. This ensures that alternative approaches to data
normalization and distance matrix calculations can be chosen by the
user.

## Usage

``` r
label_prop(full_fused_network, clusters)
```

## Arguments

- full_fused_network:

  A network made by running SNF on training and test observations
  together.

- clusters:

  A vector of assigned clusters for training observations in matching
  order as they appear in full_fused_network.

## Value

A list of cluster labels for all observations.
