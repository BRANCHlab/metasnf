# metasnf 0.3.0

## Breaking changes

* The original SNFtool function `estimateNumberOfClustersGivenGraph` has been used up to this point without specifying a parameter for `NUMC`. Consequently, final similarity matrices clustered with the default methods (spectral clustering based on eigen-gap or rotation cost heuristics) were not capable of resulting in more than 5 clusters. The default functions have been updated to span 2 clusters to 10 clusters. Users will likely see different clustering results as a result of this change. To replicate the behaviour of default spectral clustering prior to v0.3.0, users should copy the following code prior to the batch_snf command:

```
clust_algs_list <- generate_clust_algs_list(
    "spectral_eigen" = spectral_eigen_classic,
    "spectral_rot" = spectral_rot_classic
)

# Adapt below as necessary
solutions_matrix <- batch_snf(
    data_list,
    settings_matrix,
    clust_algs_list = clust_algs_list
)
```

## New functionality

* Function `remove_signal()` enables correcting a data_list linearly for confounders / unwanted signal. Vignette is available: [https://branchlab.github.io/metasnf/articles/confounders.html](https://branchlab.github.io/metasnf/articles/confounders.html).
* `batch_snf()` has new parameter `automatic_standard_normalize` to switch out the default numeric distance measures (euclidean) with standard normalized variants.

## Other changes

* Added a `NEWS.md` file to track changes to the package.

