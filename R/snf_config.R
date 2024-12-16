#' Define configuration for generating a set of SNF-based cluster solutions
#'
#' `snf_config()` constructs an SNF config object which inherits from classes
#' `snf_config` and `list`. This object is used to store all settings
#' required to transform data stored in a `data_list` class object into a 
#' space of cluster solutions by SNF. The SNF config object contains the
#' following components:
#'     1. A settings data frame (inherits from `settings_df` and `data.frame`).
#'        Data frame that stores SNF-specific hyperparameters and information
#'        about feature selection and weighting, SNF schemes, clustering
#'        algorithms, and distance metrics. Each row of the settings data frame
#'        corresponds to a distinct cluster solution.
#'     2. A clustering algorithms list (inherits from `clust_algs_list` and
#'        `list`), which stores all clustering algorithms that the settings
#'        data frame can point to. 
#'     3. A distance metrics list (inherits from `dist_metrics_list` and
#'        `list`), which stores all distance metrics that the settings data 
#'        frame can point to.
#'     4. A weights matrix (inherits from `weights_matrix`, `matrix`, and
#'        `array`'), which stores the feature weights to use prior to distance
#'        calculations. Each column of the weights matrix corresponds to a
#'        different feature in the data list and each row corresponds to a
#'        different row in the settings data frame.
#'
#' @param dl Parameter for the settings data frame. A `data_list` class object.
#'  Used to establish data frame inclusion columns (feature selection).
#' @param n_solutions Parameter for the settings data frame. Number of cluster
#'  solutions the settings data frame should store settings for.
#' @param min_removed_inputs Parameter for the settings data frame. The
#'  minimum number of input data frames that may be randomly removed. By
#'  default, 0.
#' @param max_removed_inputs The largest number of input data frames that may be
#'  randomly removed. By default, this is 1 less than all the provided input
#'  dataframes in the data list.
#' @param dropout_dist Parameter controlling how the random removal of input
#'  dataframes should occur. Can be "none" (no input dataframes are randomly
#'  removed), "uniform" (uniformly sample between min_removed_inputs and
#'  max_removed_inputs to determine number of input dataframes to remove), or
#'  "exponential" (pick number of input dataframes to remove by sampling from
#'  min_removed_inputs to max_removed_inputs with an exponential distribution;
#'  the default).
#' @param min_alpha The minimum value that the alpha hyperparameter can have.
#'  Random assigned value of alpha for each row will be obtained by uniformly
#'  sampling numbers between `min_alpha` and `max_alpha` at intervals of 0.1.
#'  Cannot be used in conjunction with the `alpha_values` parameter.
#' @param max_alpha The maximum value that the alpha hyperparameter can have.
#'  See `min_alpha` parameter. Cannot be used in conjunction with the
#'  `alpha_values` parameter.
#' @param min_k The minimum value that the k hyperparameter can have.
#'  Random assigned value of k for each row will be obtained by uniformly
#'  sampling numbers between `min_k` and `max_k` at intervals of 1.
#'  Cannot be used in conjunction with the `k_values` parameter.
#' @param max_k The maximum value that the k hyperparameter can have.
#'  See `min_k` parameter. Cannot be used in conjunction with the
#'  `k_values` parameter.
#' @param min_t The minimum value that the t hyperparameter can have.
#'  Random assigned value of t for each row will be obtained by uniformly
#'  sampling numbers between `min_t` and `max_t` at intervals of 1.
#'  Cannot be used in conjunction with the `t_values` parameter.
#' @param max_t The maximum value that the t hyperparameter can have.
#'  See `min_t` parameter. Cannot be used in conjunction with the
#'  `t_values` parameter.
#' @param alpha_values A number or numeric vector of a set of possible values
#'  that alpha can take on. Value will be obtained by uniformly sampling the
#'  vector. Cannot be used in conjunction with the `min_alpha` or `max_alpha`
#'  parameters.
#' @param k_values A number or numeric vector of a set of possible values
#'  that k can take on. Value will be obtained by uniformly sampling the
#'  vector. Cannot be used in conjunction with the `min_k` or `max_k`
#'  parameters.
#' @param t_values A number or numeric vector of a set of possible values
#'  that t can take on. Value will be obtained by uniformly sampling the
#'  vector. Cannot be used in conjunction with the `min_t` or `max_t`
#'  parameters.
#' @param possible_snf_schemes A vector containing the possible snf_schemes to
#'  uniformly randomly select from. By default, the vector contains all
#'  3 possible schemes: c(1, 2, 3). 1 corresponds to the "individual" scheme,
#'  2 corresponds to the "domain" scheme, and 3 corresponds to the "twostep"
#'  scheme.
#' @param clustering_algorithms A list of clustering algorithms to uniformly
#'  randomly pick from when clustering. When not specified, randomly select
#'  between spectral clustering using the eigen-gap heuristic and spectral
#'  clustering using the rotation cost heuristic. See ?generate_clust_algs_list
#'  for more details on running custom clustering algorithms.
#' @param continuous_distances A vector of continuous distance metrics to use
#'  when a custom distance_metrics_list is provided.
#' @param discrete_distances A vector of categorical distance metrics to use
#'  when a custom distance_metrics_list is provided.
#' @param ordinal_distances A vector of categorical distance metrics to use
#'  when a custom distance_metrics_list is provided.
#' @param categorical_distances A vector of categorical distance metrics to use
#'  when a custom distance_metrics_list is provided.
#' @param mixed_distances A vector of mixed distance metrics to use
#'  when a custom distance_metrics_list is provided.
#' @param distance_metrics_list List containing distance metrics to vary over.
#'  See ?generate_distance_metrics_list.
#' @param snf_input_weights Nested list containing weights for when SNF is
#'  used to merge individual input measures (see ?generate_snf_weights)
#' @param snf_domain_weights Nested list containing weights for when SNF is
#'  used to merge domains (see ?generate_snf_weights)
#' @param retry_limit The maximum number of attempts to generate a novel row.
#'  This function does not return matrices with identical rows. As the range of
#'  requested possible settings tightens and the number of requested rows
#'  increases, the risk of randomly generating a row that already exists
#'  increases. If a new random row has matched an existing row `retry_limit`
#'  number of times, the function will terminate.
#' @param cnt_dist_fns Parameter for the distance metrics list. A named list of
#'  distance metric functions for continuous data.
#' @param dsc_dist_fns Parameter for the distance metrics list. A named list of
#'  distance metric functions for discrete data.
#' @param ord_dist_fns Parameter for the distance metrics list. A named list of
#'  distance metric functions for ordinal data.
#' @param cat_dist_fns Parameter for the distance metrics list. A named list of
#'  distance metric functions for categorical data.
#' @param mix_dist_fns Parameter for the distance metrics list. A named list of
#'  distance metric functions for mixed-type data.
#' @inheritParams generate_settings_matrix
#' @inheritParams generate_distance_metrics_list
#' @inheritParams generate_weights_matrix
#' @inheritParams generate_clust_algs_list
#' @return An `snf_config` class object.
#' @export
snf_config <- function() {
    scl <- NULL
    scl <- validate_snf_config(scl)
    sc <- new_snf_config(scl)
}

#' Validator for snf_config class object
#' 
#' @keywords internal
#' @param scl An SNF config-like `list` class object.
#' @return If dll has a valid structure for a `data_list` class object, 
#'  returns input unchanged. Otherwise, raises an error.
validate_snf_config <- function(scl) {
    return(scl)
}

#' Constructor for `snf_config` class object
#' 
#' @keywords internal
#' @param scl An SNF config-like `list` class object.
#' @return An `snf_config` object.
new_snf_config <- function(scl) {
    stopifnot(is.list(scl))
    sc <- structure(scl, class = c("snf_config", "list"))
}
