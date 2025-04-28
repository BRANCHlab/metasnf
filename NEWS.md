# metasnf 2.1.2

- Wrap additional examples in `donttest`

# metasnf 2.1.1

- Remove excessively sized mock_sim_mats_list.rda

# metasnf 2.1.0

## Improvements

- calc_nmis now supports parallel processing, progress reported through progressr
- batch_snf_subsamples re-written to parallelize along subsamples rather than cluster solutions, now uses progressr for progress instead of verbose cat statements
- speed up parallelization test

## New data

- New mock data objects in the format of `mock_(class name)`, e.g., `mock_data_list` and `mock_ext_solutions_df`

## New functions

- Add several new S3 methods for plot, rbind, str, summary, t, c, extraction, merge, assignment, and type-coercion

## Bug fixes

- `auto_plot` output data frame doesn't duplicate cluster column
- error catching: data list sub-item name checking improvement
- double transposing `ext_solutions_df` no longer loses `sim_mats_list` attribute

## Other

- Typo fixes
- Code formatting
- Computationally intensive examples are now wrapped in `donttest` rather than commented out
- `observations()`, `summary_features()`, `features()`, `uids()` marked as internal

# metasnf 2.0.6

## Bug fixes

- fixed `rbind` for classes `solutions_df` and `ext_solutions_df` not preserving the class type of the contained `weights_matrix`

## Print formatting

- printing `solutions_df` or `ext_solutions_df` restricts output to 10 line max by default

## Other

- update for CRAN resubmission

# metasnf 2.0.5

## Bug fixes
- calc_aris (as of v2, v1 is still fine) incorrectly excluded the first observation from ARI calculations.
- merge.data_list wasn't properly integrating updated parameter names
- prevent solutions_df and ext_solutions_df from having 0 rows
- use `solution` column in `mc_manhattan_plot()` when extended solutions data frame has no MC labels

## Code formatting

- print.solutions_df title was set as print method for `weights matrix`
- replace dl_1/dl_2 with x&y for consistency in `merge.data_list()`

## New functions

- added `as.list()` for `dist_fns_list`, `clust_fns_list`, and `data_list` objects

## Performance improvements

- convert weights matrix to a regular matrix prior to printing reduces print time
- same as last commit
- weights matrix rbinding is faster when treated as a matrix

## Print formatting

- deprecated message on `generate_settings_matrix` needed paste0
- solutions data frame printing above 10 rows will default to 10 rows
- `print.solutions_df()` misprinted the number of observations in the solutions data frame

# metasnf 2.0.4

## OOP

- `merge_dls()` is superseded by `merge.data_lists()`

## Bug fixes

- `ext_solutions_df` manipulation won't drop `summary_features` and `features` attributes
- `estimate_nclust_given_graph` has more resiliency to floating point errors through tryCatch statement during eigengap quality assignment

# metasnf 2.0.3

- bugfix: `estimate_nclust_given_graph` has more resiliency to floating point errors through tryCatch loop updating eigenvalue scaling
- added functions: added `dplyr_row_slice()` functions for classes `solutions_df` and `ext_solutions_df`

# metasnf 2.0.2

## Formatting

- removed debugging dash lines from `extend_solutions()`

# metasnf 2.0.1

## Bug fixes

- `extend_solutions` was not assigning feature types properly during p-value calculations
- `rbind.ext_solutions_df` now takes `...` parameter before `reset_indices` parameter to avoid error during calls with unnamed parameters.
- `rbind.solutions_df` now takes `...` parameter before `reset_indices` parameter to avoid error during call without named parameters.
-  slicing `snf_config` object made weights matrix lose its class

# metasnf 2.0.0

## Breaking changes

* Extensive changes as a result of a transition to making use of R's S3 OOP system.

### Name changes and new classes

- data list (class `list`) -> (class `data_list`, `list`)
- solutions matrix (class `data.frame`) -> solutions data frame (class `solutions_df`, `data.frame`)
- extended solutions matrix (class `data.frame`) -> extended solutions data frame (class `ext_solutions_df`, `data.frame`)
- settings matrix -> settings data frame (class `data.frame`) -> (class `ext_solutions_df`, `data.frame`)
- distance metrics list (class `list`) -> distance functions list (class `dist_fns_list`, `list`)
- clustering algorithms list (class `list`) -> clustering functions list (class `clust_fns_list`, `list`)
- weights matrix (class `matrix`, `array`) -> (class `weights_matrix`, `matrix`, `array`)

### Function changes

- `generate_data_list()` -> `data_list()`
- Functions related to converting a solutions matrix into a data frame of cluster solutions (`get_cluster_df()`, `get_clusters()`, `get_cluster_solutions()`) now all superseded by custom transposition of `solutions_df` class objects (i.e., simply call `t()`)

### Workflow changes

- Functionality offered by the settings matrix, distance metrics list, clustering algorithms list, weights matrix, and corresponding functions (`generate_settings_matrix()`, `generate_distance_metrics_list()`, `generate_weights_matrix()`, `generate_clust_algs_list()`) now all superseded by single function `snf_config()` and the `snf_config` class object it produces
- Following derivation of a `split_vector`, either by `adjusted_rand_index_heatmap()` or `shiny_annotator()`, `solutions_df` and `ext_solutions_df` class objects can be annotated with their meta cluster labels using the function `label_meta_clusters()`. This is necessary prior to usage of `get_representative_solutions()`.
- Functions that convert non-data frame objects, like a data list, to a data frame, have been replaced with `as.data.frame()`
- Requesting similarity matrices are returned during `batch_snf` no longer changes the output structure from a solutions data frame to a list of a solutions data frame and a similarity matrix list. Instead, the similarity matrix list is added to the solutions data frame as an attribute and can be extracted using the function `sim_mats_list()`.

### Improvements

- Significant speed improvement to `calculate_coclustering()` function
- The p-value heatmap now follows a uni-color palette.
- Customized `print()` functions have been defined for all major metasnf objects.
- Examples have been added to all major metasnf functions.

# metasnf 1.1.2

* update settings matrix vignette to avoid convergence error on some seeds

# metasnf 1.1.1
* inclusion column bugfixes from 1.1.0

# metasnf 1.1.0

* Verbose parameter added to printing functions. By default set to FALSE.
* CRAN compliant @return values in documentation.

# metasnf 1.0.0

Last update before CRAN submission.

## Breaking changes

* Changing seed during settings matrix generation has been deprecated. Please manually call `set.seed` prior to `generate_settings_matrix` instead.

## Other

* Package size reduced by downscaling vignette images

# metasnf 0.7.2

## Bug fix

* Function `estimate_nclust_given_graph()` occasionally yielded incorrect number of cluster estimates as a result of improper scaling in metasnf v0.7.0. The scaling should be corrected now.

## Breaking changes

* Considerable changes have been made to the co-clustering workflow, including new heatmap and density plot.

# metasnf 0.7.1

## Possible breaking changes

* Occasionally, spectral clustering results may yield an n-cluster solution where n differed from the number of clusters requested as a parameter in the spectral clustering function itself. Now, the spectral clustering functions provided in metasnf have been updated to report the actual number of clusters in the generated solution, rather than the number of clusters that was requested

# metasnf 0.7.0

## Minor changes

* warnings provided when generating a data list with duplicate feature names
* warnings provided when using `mc_manhattan_plot()` with a data list containing duplicate feature names
* `mc_manhattan_plot()` parameter `rep_solution` replaced with more accurate name `extended_solutions_matrix` (solutions matrix with _pval columns)

## Bug fix

* `SNFtool::estimateNumberOfClustersGivenGraph()` could occasionally error out on the basis of calculating eigenvectors (eigengap heuristic) for a Laplacian with floating point values that were too small. Adapted function `estimate_nclust_given_graph()` slightly scales up Laplacian to reduce the risk of encountering this error (presumably without any change to resulting cluster number estimate)

# metasnf 0.6.8

## New functionality

* `get_matrix_order` has arguments allowing users to control which distance metric and agglomerative hierarchical clustering methods are used to sort matrices

# metasnf 0.6.7

## Minor changes

* More consistent usage of "feature" over "variable" across documentation.
* New mock ABCD dataframes - like the old ones, but without the "abcd_" prefix and with a more accurate "unique_id" UID column rather than "patient"

# metasnf 0.6.6

## New functionality

* `get_complete_uids` quickly pulls UIDs of observations with complete data from a list of dataframes

# metasnf 0.6.5

## Bug fix

* `extend_solutions` doesn't crash on multi-feature target lists

# metasnf 0.6.4

## Minor changes

* Warning message provided when subjects are dropped during `generate_data_list()`
* New `remove_missing` parameter for `generate_data_list` allowing subjects with incomplete data to remain in the data list

# metasnf 0.6.3

## Bug fixes

* ensure cluster variable is treated as factor during autoplotting
* bugfix on autoplots built from tibbles rather than dataframes

## Improvements

* Added clarity to `lp_solutions_matrix` error message when training set is not subset of full data list
* `generate_data_list` list elements now are named after their components
* added heatmap parameters to increase plotting flexibility

## New functionality

* added generic save_plot function and option to pass cluster_df directly into auto_plot (useful for label propagation)
* add `merge_data_lists` functionality to horizontally merge data lists


# metasnf 0.6.2

## Bug fixes

* `extend_solutions()` will no longer crash when a data_list has the UID column in non-first position.
* `generate_data_list()` enforces the UID column to be in first position of each dataframe.

# metasnf 0.6.1

## New functionality

* `auto_plot()` will automatically generate bar and/or jitter plots showing how features in a data_list/target_list are distributed across a single cluster solution

# metasnf 0.6.0

## New functionality

* `shiny_annotator()` function can be used to identify indices of meta clusters within an `adjusted_rand_index_heatmap`
* `adjusted_rand_index_heatmap()` now has a `split_vector` parameter that will slice a heatmap into meta clusters
* `rename_dl()` can be used to rename features in a data_list
* `manhattan_plot` has been split into `var_manhattan_plot` (key variable - all variables), `esm_manhattan_plot` (cluster solutions in an extended solutions matrix to all variables), and `mc_manhattan_plot` (like `esm_manhattan_plot`, but at the meta-cluster level)
* `get_representative_solutions` extracts max-ARI solutions from an extended solutions matrix based on a `split_vector` containing meta cluster boundaries
* `batch_nmi` calculates NMI scores (see https://branchlab.github.io/metasnf/articles/nmi_scores.html)
* `extend_solutions` will only calculate p-value summary measures (min/max/mean) for data_list passed in as a `target_list` parameter, but will also accept and calculate p-values for a data_list passed in through the `data_list` parameter
* heatmap function `adjusted_rand_index_heatmap` and `assoc_pval_heatmap` have updated parameters to improve ease of use and flexibility (including easier colour control)

## Deprecated functions

* `get_clustered_subs` has been removed (does the same thing as `get_cluster_df`)
* `get_cluster_pval` deprecated for `calc_assoc_pval`
* All functions related to target_lists specifically have been deprecated in favour of simply using `generate_data_list()` and its corresponding functions

## Name changes

* `remove_signal` has been renamed to `linear_adjust` to better reflect its function
* `summarize_distance_metrics_list` has been shortened to `summarize_dml`
* `correlation_pval_heatmap` has been renamed to `assoc_pval_heatmap`
* `calc_om_aris` has been renamed to `calc_aris`

## New vignettes

* NMI scores: https://branchlab.github.io/metasnf/articles/nmi_scores.html
* Imputations: https://branchlab.github.io/metasnf/articles/imputations.html

## Other changes

* Vignettes have been updated
* Warnings are raised if spectral clustering does not generate a cluster solution matching the number of clusters requested
* Chi-squared and `extend_solutions` p-value calculation warnings are now suppressed

# metasnf 0.5.0

## Breaking changes

* All variables and values referencing p-values have been rephrased to end in `_pval` instead of a mix of `p_val`, `pval`, and `p`.
* Removal of deprecated functions `pval_select`, `p_val_select`, `top_oms_per_cluster`, `check_subj_orders_for_lp`, `get_p`, `chi_sq_pval`,
* Function `pval_summaries`, which would calculate min/max/mean p-values, has been replaced with `summarize_pvals`
* `train_test_assign` now provides results as named list of subject vectors instead of a data.frame. `keep_split` function has been removed accordingly.

## Other changes

* `sort_subjects` parameter added to `generate_data_list` to allow for sorting of subjects in the data_list

# metasnf 0.4.6

* fix bug in extend_solutions that incorrectly assigns p-values to variable columns through grep (substring instead of exact match)

# metasnf 0.4.5

* `extend_solutions` can now also be parallelized (see ?extend_solutions)
* `remove_signal` function has `sig_digs` parameter that can be used to restrict how many significant figures are returned in the resulting residuals

# metasnf 0.4.4

* `calc_om_aris` is now MUCH faster after removing excessive calls to `as.numeric` and enabling parallel processing with `future.apply`. Thanks for the idea, Alper.

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

