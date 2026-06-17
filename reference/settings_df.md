# Build a settings data frame

The settings_df is a data frame whose rows completely specify the
hyperparameters and decisions required to transform individual input
data frames (found in a data list, see ?data_list) into a single
similarity matrix through SNF. The format of the settings data frame is
as follows:

- A column named "solution": This column is used to keep track of the
  rows and should have integer values only.

- A column named "alpha": This column contains the value of the alpha
  hyperparameter that will be used on that run of the SNF pipeline.

- A column named "k": Like above, but for the K (nearest neighbours)
  hyperparameter.

- A column named "t": Like above, but for the t (number of iterations)
  hyperparameter.

- A column named "snf_scheme": Which of 3 pre-defined schemes will be
  used to integrate the data frames of the data list into a final fused
  network. The purpose of varying these schemes is primarily to increase
  the diversity of the generated cluster solutions.

  - A value of 1 corresponds to the "individual" scheme, in which all
    data frames are directly merged by SNF into the final fused network.
    This scheme corresponds to the approach shown in the original SNF
    paper.

  - A value of 2 corresponds to the "two-step" scheme, in which all data
    frames within a domain are first merged into a domain-specific fused
    network. Next, domain-specific networks are fused once more by SNF
    into the final fused network. This scheme is useful for fairly
    re-weighting SNF pipelines with unequal numbers of data frames
    across domains.

  - A value of 3 corresponds to the "domain" scheme, in which all data
    frames within a domain are first concatenated into a single domain-
    specific data frame before being merged by SNF into the final fused
    network. This approach serves as an alternative way to re-weight SNF
    pipelines with unequal numbers of data frames across domains. You
    can learn more about this parameter here:
    https://branchlab.github.io/metasnf/articles/snf_schemes.html.

- A column named "clust_alg": Specification of which clustering
  algorithm will be applied to the final similarity matrix. By default,
  this column can take on the integer values 1 or 2, which correspond to
  spectral clustering where the number of clusters is determined by the
  eigen-gap or rotation cost heuristic respectively. You can learn more
  about this parameter here:
  https://branchlab.github.io/metasnf/articles/clustering_algorithms.html.

- A column named "cnt_dist": Specification of which distance metric will
  be used for data frames of purely continuous data. You can learn about
  this metric and its defaults here:
  https://branchlab.github.io/metasnf/articles/distance_metrics.html

- A column named "dsc_dist": Like above, but for discrete data frames.

- A column named "ord_dist": Like above, but for ordinal data frames.

- A column named "cat_dist": Like above, but for categorical data
  frames.

- A column named "mix_dist": Like above, but for mixed-type (e.g., both
  categorical and discrete) data frames.

- One column for every input data frame in the corresponding data list
  which can either have the value of 0 or 1. The name of the column
  should be formatted as "inc\_\[\]" where the square brackets are
  replaced with the name (as found in dl_summary(dl)\$"name") of each
  data frame. When 0, that data frame will be excluded from that run of
  the SNF pipeline. When 1, that data frame will be included.

## Usage

``` r
settings_df(
  dl,
  n_solutions = 0,
  min_removed_inputs = 0,
  max_removed_inputs = length(dl) - 1,
  dropout_dist = "exponential",
  min_alpha = NULL,
  max_alpha = NULL,
  min_k = NULL,
  max_k = NULL,
  min_t = NULL,
  max_t = NULL,
  alpha_values = NULL,
  k_values = NULL,
  t_values = NULL,
  possible_snf_schemes = c(1, 2, 3),
  clustering_algorithms = NULL,
  continuous_distances = NULL,
  discrete_distances = NULL,
  ordinal_distances = NULL,
  categorical_distances = NULL,
  mixed_distances = NULL,
  dfl = NULL,
  snf_input_weights = NULL,
  snf_domain_weights = NULL,
  retry_limit = 10,
  allow_duplicates = FALSE
)
```

## Arguments

- dl:

  A nested list of input data from
  [`data_list()`](https://branchlab.github.io/metasnf/reference/data_list.md).

- n_solutions:

  Number of rows to generate for the settings data frame.

- min_removed_inputs:

  The smallest number of input data frames that may be randomly removed.
  By default, 0.

- max_removed_inputs:

  The largest number of input data frames that may be randomly removed.
  By default, this is 1 less than all the provided input data frames in
  the data list.

- dropout_dist:

  Parameter controlling how the random removal of input data frames
  should occur. Can be "none" (no input data frames are randomly
  removed), "uniform" (uniformly sample between min_removed_inputs and
  max_removed_inputs to determine number of input data frames to
  remove), or "exponential" (pick number of input data frames to remove
  by sampling from min_removed_inputs to max_removed_inputs with an
  exponential distribution; the default).

- min_alpha:

  The minimum value that the alpha hyperparameter can have. Random
  assigned value of alpha for each row will be obtained by uniformly
  sampling numbers between `min_alpha` and `max_alpha` at intervals of
  0.1. Cannot be used in conjunction with the `alpha_values` parameter.

- max_alpha:

  The maximum value that the alpha hyperparameter can have. See
  `min_alpha` parameter. Cannot be used in conjunction with the
  `alpha_values` parameter.

- min_k:

  The minimum value that the k hyperparameter can have. Random assigned
  value of k for each row will be obtained by uniformly sampling numbers
  between `min_k` and `max_k` at intervals of 1. Cannot be used in
  conjunction with the `k_values` parameter.

- max_k:

  The maximum value that the k hyperparameter can have. See `min_k`
  parameter. Cannot be used in conjunction with the `k_values`
  parameter.

- min_t:

  The minimum value that the t hyperparameter can have. Random assigned
  value of t for each row will be obtained by uniformly sampling numbers
  between `min_t` and `max_t` at intervals of 1. Cannot be used in
  conjunction with the `t_values` parameter.

- max_t:

  The maximum value that the t hyperparameter can have. See `min_t`
  parameter. Cannot be used in conjunction with the `t_values`
  parameter.

- alpha_values:

  A number or numeric vector of a set of possible values that alpha can
  take on. Value will be obtained by uniformly sampling the vector.
  Cannot be used in conjunction with the `min_alpha` or `max_alpha`
  parameters.

- k_values:

  A number or numeric vector of a set of possible values that k can take
  on. Value will be obtained by uniformly sampling the vector. Cannot be
  used in conjunction with the `min_k` or `max_k` parameters.

- t_values:

  A number or numeric vector of a set of possible values that t can take
  on. Value will be obtained by uniformly sampling the vector. Cannot be
  used in conjunction with the `min_t` or `max_t` parameters.

- possible_snf_schemes:

  A vector containing the possible snf_schemes to uniformly randomly
  select from. By default, the vector contains all 3 possible schemes:
  c(1, 2, 3). 1 corresponds to the "individual" scheme, 2 corresponds to
  the "domain" scheme, and 3 corresponds to the "two-step" scheme.

- clustering_algorithms:

  (DEPRECATED) Clustering algorithms to uniformly randomly pick from
  when clustering. See ?clust_fns_list for more details.

- continuous_distances:

  A vector of continuous distance metrics to use when a custom
  dist_fns_list is provided.

- discrete_distances:

  A vector of categorical distance metrics to use when a custom
  dist_fns_list is provided.

- ordinal_distances:

  A vector of categorical distance metrics to use when a custom
  dist_fns_list is provided.

- categorical_distances:

  A vector of categorical distance metrics to use when a custom
  dist_fns_list is provided.

- mixed_distances:

  A vector of mixed distance metrics to use when a custom dist_fns_list
  is provided.

- dfl:

  List containing distance metrics to vary over. See
  ?generate_dist_fns_list.

- snf_input_weights:

  Nested list containing weights for when SNF is used to merge
  individual input measures (see ?generate_snf_weights)

- snf_domain_weights:

  Nested list containing weights for when SNF is used to merge domains
  (see ?generate_snf_weights)

- retry_limit:

  The maximum number of attempts to generate a novel row. This function
  does not return matrices with identical rows. As the range of
  requested possible settings tightens and the number of requested rows
  increases, the risk of randomly generating a row that already exists
  increases. If a new random row has matched an existing row
  `retry_limit` number of times, the function will terminate.

- allow_duplicates:

  If TRUE, enables creation of a settings data frame with duplicate
  non-feature weighting related hyperparameters. This function should
  only be used when paired with a custom weights matrix that has
  non-duplicate rows.

## Value

A settings data frame
