# metasnf 0.4.3

* Reformatting of `extend_solutions` to better handle extreme p-values (e.g. infinity)
* Replacement of `p_val_select` with `pval_select` which can also return negative-log p-values

# metasnf 0.4.2

## Bug fixes

* `generate_data_list` correctly errors when components are only partially named (resolves https://github.com/BRANCHlab/metasnf/issues/10)

# metasnf 0.4.1

## Breaking changes

* `lp_row` function has been replaced by `lp_solutions_matrix`. The new function is order agnostic: full data lists can be constructed without any restriction on how training and testing set subjects are sorted. Subjects present in the provided solutions matrix to propagate are assumed to be the training subjects.

## New functionality

* `calc_om_aris` now has `progress` parameter. When set to true and used in conjunction with `progressr::with_progress()`, a progress bar is shown for the calculations. Learn more with `?calc_om_aris`.

## Bug fixes

* `grepl` instead of `grep` used in `extend_solutions` to reduce errors when no chi-squared warning occurs


## Other changes

* A vignette specifically for label propagation has been added
* Full removal of several previously deprecated functions
* Minor source code reformatting

# metasnf 0.4.0

## New functionality

* Parallel processing is now available! Check out the vignette here: https://branchlab.github.io/metasnf/articles/parallel_processing.html

# metasnf 0.3.3

## Breaking changes

* input_wt and domain_wt are removed from settings_matrix and rest of package - weighting at this level is no longer planned. This will result in altered settings matrices, but only superficially - the columns "input_wt" and "domain_wt" will be missing, but had no effect on the SNF prior to this patch anyway.

# metasnf 0.3.2

* `keep_split` will preserve observations who were assigned a split but were not present in the dataframe being split. Instead of being removed, those observations will have NA values.

# metasnf 0.3.1

## Bug fixes

* fixed `fraction_clustered_together` crashing when a cluster was assigned to only a single observation
* fixed `fraction_clustered_together` not running due to bracket typo when evaluating length of the data_list

## New functionality

* `correlation_pval_heatmap` function can have significance stars disabled with `significance_stars` parameter

## Other changes

* pkgdown site now has google site verification code


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

* Added "workspace=2e7" parameter to `fisher_exact_pval` function to avoid "FEXACT" error (like here https://github.com/Lagkouvardos/Rhea/issues/17). Impact on results is expected to be negligible.

## New functionality

* Function `remove_signal()` enables correcting a data_list linearly for confounders / unwanted signal. Vignette is available: [https://branchlab.github.io/metasnf/articles/confounders.html](https://branchlab.github.io/metasnf/articles/confounders.html).
* `batch_snf()` has new parameter `automatic_standard_normalize` to switch out the default numeric distance measures (euclidean) with standard normalized variants.

## Other changes

* Added a `NEWS.md` file to track changes to the package.

