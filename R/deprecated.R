#' Generate a list of distance metrics
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Deprecated function for building a distance metrics list. Please use
#' `dist_fns_list()` instead.
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
        "Please use `dist_fns_list()` instead."
    )
    dist_fns_list(
        cnt_dist_fns = continuous_distances,
        dsc_dist_fns = discrete_distances,
        ord_dist_fns = ordinal_distances,
        cat_dist_fns = categorical_distances,
        mix_dist_fns = mixed_distances,
        use_default_dist_fns = keep_defaults
    )
}

#' Generate a clustering algorithms list
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Deprecated function for building a clustering algorithms list. Please use
#' `dist_fns_list()` instead.
#'
#' @param ... An arbitrary number of named clustering functions
#' @param disable_base If TRUE, do not prepend the base clustering algorithms
#'  (spectral_eigen and spectral_rot, which apply spectral clustering and use
#'  the eigen-gap and rotation cost heuristics respectively for determining
#'  the number of clusters in the graph.
#' @return A list of clustering algorithm functions that can
#'  be passed into the batch_snf and generate_settings_list functions.
generate_clust_algs_list <- function(..., disable_base = FALSE) {
    metasnf_deprecated(
        "2.0.0",
        "Please use `clust_fns_list()` instead."
    )
    clust_fns_list(
        clust_fns = list(...),
        use_default_clust_fns  = !disable_base
    )
}

#' Build a settings data frame
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Deprecated function for building a settings matrix. Please use
#' `settings_df()` instead.
#'
#' @param ... Arguments used to generate a settings matrix.
#' @return Raises a deprecated error.
#' @export
generate_settings_matrix <- function(...) {
    metasnf_deprecated(
        "2.0.0",
        "Settings and hyperparameters are now handled in `snf_config` class o",
        "bjects. To generate an SNF config object, please use `snf_config()`."
    )
}


#' Convert a data list into a data frame
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Defunct function for converting a data list into a data frame. Please
#'  use `as.data.frame()` instead.
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#' @return A "data.frame"-formatted version of the provided data list.
#' @export
collapse_dl <- function(data_list) {
    metasnf_deprecated(
        "2.0.0",
        paste0(
            "Please ensure your data list is a `data_list` class object made ",
            "by `data_list()` rather than `generate_data_list()` and then cal",
            "l `as.data.frame` instead."
        )
    )
    if (inherits(data_list, "data_list")) {
        dl_df <- as.data.frame(data_list) 
        return(dl_df)
    }
}

#' Summarize a data list
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Defunct function for summarizing a data list. Please
#'  use `summary()` instead.
#'
#' @param dl A nested list of input data from `data_list()`.
#' @param scope The level of detail for the summary. Options are:
#' - "component" (default): One row per component (dataframe) in the data list.
#' - "feature": One row for each feature in the data list.
#' @return data.frame class object summarizing all components (or features if
#' scope == "component").
#' @export
summarize_dl <- function(data_list, scope = "component") {
    metasnf_deprecated("2.0.0", "Please use `summary()` instead.")
    if (inherits(data_list, "data_list")) {
        dl_summary <- summary(data_list) 
        return(dl_summary)
    }
}

#' Variable-level summary of a data list
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Defunct function to summarize a data list. Please use `summary()` with
#' argument `scope = "feature"` instead.
#'
#' @param dl A nested list of input data from `data_list()`.
#' @return variable_level_summary A dataframe containing the name, type, and
#' domain of every variable in a data list.
#'
#' @export
dl_variable_summary <- function(dl) {
    metasnf_deprecated(
        "2.0.0",
        "Please use `summary()` with `scope = \"feature\"` instead."
    )
    if (inherits(data_list, "data_list")) {
        dl_summary <- summary(data_list, scope = "feature") 
        return(dl_summary)
    }
}

