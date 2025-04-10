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
#'     2. A clustering algorithms list (inherits from `clust_fns_list` and
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
#' @param sdf A `settings_df` class object. Overrides settings data frame
#'  related parameters.
#' @param dfl A `dist_fns_list` class object. Overrides distance functions list
#'  related parameters.
#' @param cfl A `clust_fns_list` class object. Overrides clustering functions
#'  list related parameters.
#' @param wm A `weights_matrix` class object. Overrides weights matrix
#'  related parameters.
#' @inheritParams settings_df
#' @inheritParams dist_fns_list
#' @inheritParams clust_fns_list
#' @inheritParams weights_matrix
#' @return An `snf_config` class object.
#' @export
#' @examples
#' # Simple random config for 5 cluster solutions
#' input_dl <- data_list(
#'     list(anxiety, "anxiety", "behaviour", "ordinal"),
#'     list(depress, "depressed", "behaviour", "ordinal"),
#'     uid = "unique_id"
#' )
#' my_sc <- snf_config(
#'     dl = input_dl,
#'     n_solutions = 5
#' )
#' 
#' # specifying possible K range
#' my_sc <- snf_config(
#'     dl = input_dl,
#'     n_solutions = 5,
#'     min_k = 20,
#'     max_k = 40
#' )
#' 
#' # Random feature weights across from uniform distribution
#' my_sc <- snf_config(
#'     dl = input_dl,
#'     n_solutions = 5,
#'     min_k = 20,
#'     max_k = 40,
#'     weights_fill = "uniform"
#' )
#' 
#' # Specifying custom pre-built clustering and distance functions
#' # - Random alternation between 2-cluster and 5-cluster solutions
#' # - When continuous or discrete data frames are being processed,
#' #   randomly alternate between standardized/normalized Euclidean
#' #   distance and regular Euclidean distance
#' my_sc <- snf_config(
#'     dl = input_dl,
#'     n_solutions = 5,
#'     min_k = 20,
#'     max_k = 40,
#'     weights_fill = "uniform",
#'     clust_fns = list(
#'         "two_cluster_spectral" = spectral_two,
#'         "five_cluster_spectral" = spectral_five
#'     ),
#'     cnt_dist_fns = list(
#'          "euclidean" = euclidean_distance,
#'          "std_nrm_euc" = sn_euclidean_distance
#'     ),
#'     dsc_dist_fns = list(
#'          "euclidean" = euclidean_distance,
#'          "std_nrm_euc" = sn_euclidean_distance
#'     )
#' )
snf_config <- function(dl = NULL,
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
                       weights_fill = "ones") {
    if (is.null(dfl)) {
        dfl <- dist_fns_list(
            cnt_dist_fns = cnt_dist_fns,
            dsc_dist_fns = dsc_dist_fns,
            ord_dist_fns = ord_dist_fns,
            cat_dist_fns = cat_dist_fns,
            mix_dist_fns = mix_dist_fns,
            automatic_standard_normalize = automatic_standard_normalize,
            use_default_dist_fns = use_default_dist_fns
        )
    }
    if (is.null(cfl)) {
        cfl <- clust_fns_list(
            clust_fns = clust_fns,
            use_default_clust_fns = use_default_clust_fns
        )
    }
    if (is.null(wm)) {
        wm <- weights_matrix(
            dl = dl,
            n_solutions = n_solutions,
            weights_fill = weights_fill
        )
    }
    if (is.null(sdf)) {
        sdf <- settings_df(
            dl,
            n_solutions = n_solutions,
            min_removed_inputs = min_removed_inputs,
            max_removed_inputs = max_removed_inputs,
            dropout_dist = dropout_dist,
            min_alpha = min_alpha,
            max_alpha = max_alpha,
            min_k = min_k,
            max_k = max_k,
            min_t = min_t,
            max_t = max_t,
            alpha_values = alpha_values,
            k_values = k_values,
            t_values = t_values,
            possible_snf_schemes = possible_snf_schemes,
            clustering_algorithms = clustering_algorithms,
            continuous_distances = continuous_distances,
            discrete_distances = discrete_distances,
            ordinal_distances = ordinal_distances,
            categorical_distances = categorical_distances,
            mixed_distances = mixed_distances,
            dfl = dfl,
            snf_input_weights = snf_input_weights,
            snf_domain_weights = snf_domain_weights,
            retry_limit = retry_limit
        )
    }
    scl <- list(
        "settings_df" = sdf,
        "dist_fns_list" = dfl,
        "clust_fns_list" = cfl,
        "weights_matrix" = wm
    )
    scl <- validate_snf_config(scl)
    sc <- new_snf_config(scl)
    return(sc)
}

#' Validator for snf_config class object
#' 
#' @keywords internal
#' @param scl An SNF config-like `list` class object.
#' @return If dll has a valid structure for a `data_list` class object, 
#'  returns input unchanged. Otherwise, raises an error.
validate_snf_config <- function(scl) {
    class(scl) <- setdiff(class(scl), "snf_config")
    # 1. Correct length
    if (!length(scl) == 4) {
        metasnf_error("SNF config must contain 4 items.")
    }
    # 2. Correct items
    items <- c(
        "settings_df", "dist_fns_list", "clust_fns_list", "weights_matrix"
    )
    if (!all(items %in% names(scl))) {
        metasnf_error(
            "SNF config must contain `settings_df`, `dist_fns_list`,",
            " `clust_fns_list`, and `weights_matrix`."
        )
    }
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
    attributes(sc)$"n_solutions" <- nrow(sc$"settings_df")
    sc
}
