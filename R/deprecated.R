#' Generate a list of distance metrics
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Deprecated function for building a distance metrics list. Please use
#' `distance_metrics_list()` instead.
#'
#' @param continuous_distances A named list of distance metric functions
#' @param discrete_distances A named list of distance metric functions
#' @param ordinal_distances A named list of distance metric functions
#' @param categorical_distances A named list of distance metric functions
#' @param mixed_distances A named list of distance metric functions
#' @param keep_defaults If TRUE (default), prepend the base distance metrics
#'  (euclidean and standard normalized euclidean)
#'
#' @return A nested and named list of distance metrics functions.
#' @export
generate_distance_metrics_list <- function(continuous_distances = NULL,
                                           discrete_distances = NULL,
                                           ordinal_distances = NULL,
                                           categorical_distances = NULL,
                                           mixed_distances = NULL,
                                           keep_defaults = TRUE) {
    metasnf_deprecated(
        "2.0.0",
        "Please use `distance_metrics_list()` instead."
    )
    distance_metrics_list(
        cnt_dist_fns = continuous_distances,
        dsc_dist_fns = discrete_distances,
        ord_dist_fns = ordinal_distances,
        cat_dist_fns = categorical_distances,
        mix_dist_fns = mixed_distances,
        use_default_dist_fns = keep_defaults
    )
}
