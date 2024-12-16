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
#' @inheritParams generate_settings_matrix
#' @inheritParams generate_distance_metrics_list
#' @inheritParams generate_weights_matrix
#' @inheritParams generate_clust_algs_list
#' @return An `snf_config` class object.
#' @export
snf_config <- function(dl,
                       nrows = 0,
                       min_removed_inputs = 0,
                       max_removed_inputs = length(
                           dl
                       ) - 1,
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
                       distance_metrics_list = NULL,
                       snf_input_weights = NULL,
                       snf_domain_weights = NULL,
                       retry_limit = 10,
                       cnt_dist_fns = NULL,
                       dsc_dist_fns = NULL,
                       ord_dist_fns = NULL,
                       cat_dist_fns = NULL,
                       mix_dist_fns = NULL,
                       keep_defaults = TRUE) {
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
