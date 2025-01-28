#' Generate a list of distance metrics
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Deprecated function for building a distance metrics list. Please use
#' `dist_fns_list()` (or better yet, `snf_config()`) instead.
#'
#' @param continuous_distances A named list of distance metric functions
#' @param discrete_distances A named list of distance metric functions
#' @param ordinal_distances A named list of distance metric functions
#' @param categorical_distances A named list of distance metric functions
#' @param mixed_distances A named list of distance metric functions
#' @param keep_defaults If TRUE (default), prepend the base distance metrics
#'  (euclidean and standard normalized euclidean)
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
#' `clust_fns_list()` (or better yet, `snf_config()`) instead.
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
        "Please use `snf_config()` or `clust_fns_list()` instead."
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
#' @param data_list A nested list of input data from `data_list()`.
#' @param scope The level of detail for the summary. Options are:
#' - "component" (default): One row per component (data frame) in the data list.
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
#' @return variable_level_summary A data frame containing the name, type, and
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

#' Heatmap of pairwise adjusted rand indices between solutions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Defunct function to create an ARI heatmap. Please use
#' `meta_cluster_heatmap()` instead.
#'
#' @param aris Matrix of adjusted rand indices from `calc_aris()`
#' @param order Numeric vector containing row order of the heatmap.
#' @param cluster_rows Whether rows should be clustered.
#' @param cluster_columns Whether columns should be clustered.
#' @param log_graph If TRUE, log transforms the graph.
#' @param scale_diag Method of rescaling matrix diagonals. Can be "none"
#'  (don't change diagonals), "mean" (replace diagonals with average value of
#'  off-diagonals), or "zero" (replace diagonals with 0).
#' @param min_colour Colour used for the lowest value in the heatmap.
#' @param max_colour Colour used for the highest value in the heatmap.
#' @param col Colour ramp to use for the heatmap.
#' @param ... Additional parameters passed to `similarity_matrix_heatmap()`,
#'  the function that this function wraps.
#' @return Returns a heatmap (class "Heatmap" from package ComplexHeatmap)
#'  that displays the pairwise adjusted Rand indices (similarities) between
#'  the cluster solutions of the provided solutions data frame.
#' @export
adjusted_rand_index_heatmap <- function(aris,
                                        order = NULL,
                                        cluster_rows = FALSE,
                                        cluster_columns = FALSE,
                                        log_graph = FALSE,
                                        scale_diag = "none",
                                        min_colour = "#282828",
                                        max_colour = "firebrick2",
                                        col = circlize::colorRamp2(
                                            c(min(aris), max(aris)),
                                            c(min_colour, max_colour)
                                        ),
                                        ...) {
    metasnf_deprecated("2.0.0", "Please use `meta_cluster_heatmap()` instead.")
    heatmap <- meta_cluster_heatmap(
        aris = aris,
        order = order,
        cluster_rows = cluster_rows,
        cluster_columns = cluster_columns,
        log_graph = log_graph,
        scale_diag = scale_diag,
        col = col,
        ...
    )
    return(heatmap)
}

#' Extract cluster membership information from a sol_df
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Deprecated function for building extracting cluster solutions from a
#' solutions data frame. Please use `t()` instead.
#'
#' This function takes in a solutions data frame and returns a data frame containing
#' the cluster assignments for each uid. It is similar to
#' '`get_clusters()`, which takes one solutions data frame row and returns a vector
#' of cluster assignments' and `get_cluster_df()`, which takes a solutions
#' matrix with only one row and returns a data frame with two columns: "cluster"
#' and "uid" (the UID of the observation).
#'
#' @param sol_df A sol_df.
#' @return A "data.frame" object where each row is an
#'  observation and each column (apart from the uid column) indicates
#'  the cluster that observation was assigned to for the corresponding
#'  solutions data frame row.
#' @export
get_cluster_solutions <- function(sol_df) {
    metasnf_deprecated("2.0.0", "Please use `t()` instead.")
    if (inherits(sol_df, "solutions_df")) {
        return(t(sol_df))
    }
}

#' Extract cluster membership information from one solutions data frame row
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Deprecated function for building extracting cluster solutions from a
#' solutions data frame. Please use `t()` instead.
#'
#' This function takes in a single row of a solutions data frame and returns a
#' data frame containing the cluster assignments for each uid. It is
#' similar to `get_clusters()`, which takes one solutions data frame row and
#' returns a vector of cluster assignments' and `get_cluster_solutions()`,
#' which takes a solutions data frame with any number of rows and returns a
#' data frame indicating the cluster assignments for each of those rows.
#'
#' @param sol_df_row One row from a solutions data frame.
#' @return cluster_df data frame of cluster and uid.
#' @export
get_cluster_df <- function(sol_df_row) {
    metasnf_deprecated("2.0.0", "Please use `t()` instead.")
    if (inherits(sol_df_row, "solutions_df")) {
        return(t(sol_df_row))
    }
}

#' Extract cluster membership vector from one solutions data frame row
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Deprecated function for building extracting cluster solutions from a
#' solutions data frame. Please use `t()` instead.
#'
#' This function takes in a single row of a solutions data frame and returns a
#' vector containing the cluster assignments for each observation. It is
#' similar to `get_cluster_df()`, which takes a solutions data frame with only one
#' row and returns a data frame with two columns: "cluster" and "uid"
#' '(the UID of the observation) and `get_cluster_solutions()`, which takes a
#' solutions data frame with any number of rows and returns a data frame indicating
#' the cluster assignments for each of those rows.
#'
#' @param sol_df_row Output matrix row.
#' @return clusters Vector of assigned clusters.
#' @export
get_clusters <- function(sol_df_row) {
    metasnf_deprecated("2.0.0", "Please use `t()` instead.")
    if (inherits(sol_df_row, "solutions_df")) {
        return(t(sol_df_row))
    }
}

#' Extract UIDs from a data list
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Deprecated function for extracting UIDs from a data list.
#' Please use `uids()` instead.
#'
#' @param dl A nested list of input data from `data_list()`.
#' @param prefix If TRUE, preserves the "uid_" prefix added to UIDs when
#'  creating a data list.
#' @return A character vector of the UID labels contained in a data list.
#' @export
get_dl_uids <- function(dl, prefix = FALSE) {
    metasnf_deprecated("2.0.0", "Please use `uids()` instead.")
    return(uids(dl))
}

