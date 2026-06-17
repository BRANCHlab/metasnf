# Define configuration for generating a set of SNF-based cluster solutions

`snf_config()` constructs an SNF config object which inherits from
classes `snf_config` and `list`. This object is used to store all
settings required to transform data stored in a `data_list` class object
into a space of cluster solutions by SNF. The SNF config object contains
the following components: 1. A settings data frame (inherits from
`settings_df` and `data.frame`). Data frame that stores SNF-specific
hyperparameters and information about feature selection and weighting,
SNF schemes, clustering algorithms, and distance metrics. Each row of
the settings data frame corresponds to a distinct cluster solution. 2. A
clustering algorithms list (inherits from `clust_fns_list` and `list`),
which stores all clustering algorithms that the settings data frame can
point to. 3. A distance metrics list (inherits from `dist_metrics_list`
and `list`), which stores all distance metrics that the settings data
frame can point to. 4. A weights matrix (inherits from `weights_matrix`,
`matrix`, and `array`'), which stores the feature weights to use prior
to distance calculations. Each column of the weights matrix corresponds
to a different feature in the data list and each row corresponds to a
different row in the settings data frame.

## Usage

``` r
snf_config(
  dl = NULL,
  sdf = NULL,
  dfl = NULL,
  cfl = NULL,
  wm = NULL,
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
  snf_input_weights = NULL,
  snf_domain_weights = NULL,
  retry_limit = 10,
  cnt_dist_fns = NULL,
  dsc_dist_fns = NULL,
  ord_dist_fns = NULL,
  cat_dist_fns = NULL,
  mix_dist_fns = NULL,
  automatic_standard_normalize = FALSE,
  use_default_dist_fns = FALSE,
  clust_fns = NULL,
  use_default_clust_fns = FALSE,
  weights_fill = "ones"
)
```

## Arguments

- dl:

  A nested list of input data from
  [`data_list()`](https://branchlab.github.io/metasnf/reference/data_list.md).

- sdf:

  A `settings_df` class object. Overrides settings data frame related
  parameters.

- dfl:

  A `dist_fns_list` class object. Overrides distance functions list
  related parameters.

- cfl:

  A `clust_fns_list` class object. Overrides clustering functions list
  related parameters.

- wm:

  A `weights_matrix` class object. Overrides weights matrix related
  parameters.

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

- cnt_dist_fns:

  A named list of continuous distance metric functions.

- dsc_dist_fns:

  A named list of discrete distance metric functions.

- ord_dist_fns:

  A named list of ordinal distance metric functions.

- cat_dist_fns:

  A named list of categorical distance metric functions.

- mix_dist_fns:

  A named list of mixed distance metric functions.

- automatic_standard_normalize:

  If TRUE, will automatically use standard normalization prior to
  calculation of any numeric distances. This parameter overrides all
  other distance functions list-related parameters.

- use_default_dist_fns:

  If TRUE, prepend the base distance metrics (euclidean distance for
  continuous, discrete, and ordinal data and gower distance for
  categorical and mixed data) to the resulting distance metrics list.

- clust_fns:

  A list of named clustering functions

- use_default_clust_fns:

  If TRUE, prepend the base clustering algorithms (spectral_eigen and
  spectral_rot, which apply spectral clustering and use the eigen-gap
  and rotation cost heuristics respectively for determining the number
  of clusters in the graph) to clust_fns.

- weights_fill:

  String indicating what to populate generate rows with. Can be "ones"
  (default; fill matrix with 1), "uniform" (fill matrix with uniformly
  distributed random values), or "exponential" (fill matrix with
  exponentially distributed random values).

## Value

An `snf_config` class object.

## Examples

``` r
# Simple random config for 5 cluster solutions
input_dl <- data_list(
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    list(depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)
#> ℹ 38 observations dropped due to incomplete data.
my_sc <- snf_config(
    dl = input_dl,
    n_solutions = 5
)
#> ℹ No distance functions specified. Using defaults.
#> ℹ No clustering functions specified. Using defaults.

# specifying possible K range
my_sc <- snf_config(
    dl = input_dl,
    n_solutions = 5,
    min_k = 20,
    max_k = 40
)
#> ℹ No distance functions specified. Using defaults.
#> ℹ No clustering functions specified. Using defaults.

# Random feature weights across from uniform distribution
my_sc <- snf_config(
    dl = input_dl,
    n_solutions = 5,
    min_k = 20,
    max_k = 40,
    weights_fill = "uniform"
)
#> ℹ No distance functions specified. Using defaults.
#> ℹ No clustering functions specified. Using defaults.

# Specifying custom pre-built clustering and distance functions
# - Random alternation between 2-cluster and 5-cluster solutions
# - When continuous or discrete data frames are being processed,
#   randomly alternate between standardized/normalized Euclidean
#   distance and regular Euclidean distance
my_sc <- snf_config(
    dl = input_dl,
    n_solutions = 5,
    min_k = 20,
    max_k = 40,
    weights_fill = "uniform",
    clust_fns = list(
        "two_cluster_spectral" = spectral_two,
        "five_cluster_spectral" = spectral_five
    ),
    cnt_dist_fns = list(
         "euclidean" = euclidean_distance,
         "std_nrm_euc" = sn_euclidean_distance
    ),
    dsc_dist_fns = list(
         "euclidean" = euclidean_distance,
         "std_nrm_euc" = sn_euclidean_distance
    )
)
```
