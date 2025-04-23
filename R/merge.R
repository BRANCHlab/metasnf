#' @export
merge.ari_matrix <- function(x, y, ...) {
    metasnf_error(
        "The merge function is not applicable to `ari_matrix` class objects."
    )
}

#' Merge `clust_fns_list` objects
#'
#' @param x The first `clust_fns_list` object to merge.
#' @param y The second `clust_fns_list` object to merge.
#' @param ... Additional arguments (not used).
#' @return A new `clust_fns_list` object containing the merged clustering
#'  functions.
#' @export
merge.clust_fns_list <- function(x, y, ...) {
    args <- list(...)
    if (length(args) > 0) {
        metasnf_error(
            "The merge function does not accept more than 2 arguments."
        )
    }
    merged_cfl <- c(x, y)
    class(merged_cfl) <- c("clust_fns_list", "list")
    merged_cfl
}

#' Merge observations between two compatible data lists
#'
#' Join two data lists with the same components (data frames) but separate
#' observations. To instead merge two data lists that have the same
#' observations but different components, simply use `c()`.
#'
#' @param x The first data list to merge.
#' @param y The second data list to merge.
#' @param ... Additional arguments passed into merge function.
#' @return A data list ("list"-class object) containing the observations of
#'  both provided data lists.
#' @export
merge.data_list <- function(x, y, ...) {
    x_names <- summary(x)$"name"
    y_names <- summary(y)$"name"
    names(x) <- x_names
    names(y) <- y_names
    if (!identical(sort(x_names), sort(y_names))) {
        metasnf_error(
            "The two data lists must have identical components."
        )
    }
    merged_data_list <- lapply(
        x_names,
        function(c) {
            x[[c]]$"data" <- dplyr::bind_rows(
                x[[c]]$"data",
                y[[c]]$"data"
            )
            x[[c]]$"data" <- dplyr::arrange(
                x[[c]]$"data", 
                x[[c]]$"data"$"uid"
            )
            return(x[[c]])
        }
    )
    names(merged_data_list) <- x_names
    merged_data_list <- as_data_list(merged_data_list)
    return(merged_data_list)
}

#' Merge `dist_fns_list` objects
#'
#' @param x The first `clust_fns_list` object to merge.
#' @param y The second `clust_fns_list` object to merge.
#' @param ... Additional arguments (not used).
#' @return A new `clust_fns_list` object containing the merged clustering
#'  functions.
#' @export
merge.dist_fns_list <- function(x, y, ...) {
    args <- list(...)
    if (length(args) > 0) {
        metasnf_error(
            "The merge function for `dist_fns_list` objects",
            " does not accept more than 2 arguments."
        )
    }
    cnt_list <- c(x$"cnt_dist_fns", y$"cnt_dist_fns")
    names(cnt_list) <- c(names(x$"cnt_dist_fns"), names(y$"cnt_dist_fns"))
    dsc_list <- c(x$"dsc_dist_fns", y$"dsc_dist_fns")
    names(dsc_list) <- c(names(x$"dsc_dist_fns"), names(y$"dsc_dist_fns"))
    ord_list <- c(x$"ord_dist_fns", y$"ord_dist_fns")
    names(ord_list) <- c(names(x$"ord_dist_fns"), names(y$"ord_dist_fns"))
    cat_list <- c(x$"cat_dist_fns", y$"cat_dist_fns")
    names(cat_list) <- c(names(x$"cat_dist_fns"), names(y$"cat_dist_fns"))
    mix_list <- c(x$"mix_dist_fns", y$"mix_dist_fns")
    names(mix_list) <- c(names(x$"mix_dist_fns"), names(y$"mix_dist_fns"))
    dfl <- dist_fns_list(
        cnt_dist_fns = cnt_list[!duplicated(cnt_list)],
        dsc_dist_fns = dsc_list[!duplicated(dsc_list)],
        ord_dist_fns = ord_list[!duplicated(ord_list)],
        cat_dist_fns = cat_list[!duplicated(cat_list)],
        mix_dist_fns = mix_list[!duplicated(mix_list)],
        use_default_dist_fns = FALSE
    )
    merged_cfl <- c(x, y)
    class(merged_cfl) <- c("dist_fns_list", "list")
    merged_cfl
}

#' Merge `ext_solutions_df` objects
#'
#' @param x The first `ext_solutions_df` object to merge.
#' @param y The second `ext_solutions_df` object to merge.
#' @param ... Additional arguments (not used).
#' @return Error message indicating that the merge function is not applicable
#' to `ext_solutions_df` objects.
#' @export
merge.ext_solutions_df <- function(x, y, ...) {
    metasnf_error(
        "The merge function is not applicable to `ext_solutions_df` class",
        " objects. To combine two `ext_solutions_df` objects, use `rbind()`."
    )
}

#' Merge `settings_df` objects
#'
#' @param x The first `settings_df` object to merge.
#' @param y The second `settings_df` object to merge.
#' @param ... Additional arguments (not used).
#' @return Error message indicating that the merge function is not applicable
#' to `settings_df` objects.
#' @export
merge.settings_df <- function(x, y, ...) {
    metasnf_error(
        "The merge function is not applicable to `settings_df` class",
        " objects. To combine two `settings_df` objects, use `rbind()`."
    )
}

#' Merge `sim_mats_list` objects
#'
#' @param x The first `sim_mats_list` object to merge.
#' @param y The second `sim_mats_list` object to merge.
#' @param ... Additional arguments (not used).
#' @return A merged `sim_mats_list` object containing the similarity matrices
#' from both input objects.
#' @export
merge.sim_mats_list <- function(x, y, ...) {
    if (colnames(x[[1]]) == colnames(y[[1]])) {
        return(c(x, y))
    } else {
        metasnf_error(
            "The two sim_mats_list objects must have identical column names.",
            " Please check the input objects."
        )
    }
}

#' Merge method for SNF config objects
#'
#' @param x SNF config to merge.
#' @param y SNF config to merge.
#' @param reset_indices If TRUE (default), re-labels the "solutions" indices in
#'  the config from 1 to the number of defined settings.
#' @param ... Additional arguments passed into merge function.
#' @return An SNF config combining the rows of both prior configurations.
#' @export
merge.snf_config <- function(x, y, reset_indices = TRUE, ...) {
    if (!identical(x$"dist_fns_list", y$"dist_fns_list")) {
        metasnf_error(
            "SNF configs must have identical distance functions lists to be",
            " merged."
        )
    }
    if (!identical(x$"clust_fns_list", y$"clust_fns_list")) {
        metasnf_error(
            "SNF configs must have identical clustering functions lists to be",
            " merged."
        )
    }
    new_sdf <- rbind(x$"settings_df", y$"settings_df")
    new_wm <- rbind(
        as.matrix(x$"weights_matrix"),
        as.matrix(y$"weights_matrix")
    ) |>
        as_weights_matrix()
    x$"settings_df" <- new_sdf
    x$"weights_matrix" <- new_wm
    if (reset_indices) {
        x$"settings_df"$"solution" <- seq_len(nrow(x$"settings_df"))
    }
    x <- validate_snf_config(x)
    x <- new_snf_config(x)
    return(x)
}

#' Merge `solutions_df` objects
#'
#' @param x The first `solutions_df` object to merge.
#' @param y The second `solutions_df` object to merge.
#' @param ... Additional arguments (not used).
#' @return Error message indicating that the merge function is not applicable
#' to `solutions_df` objects.
#' @export
merge.solutions_df <- function(x, y, ...) {
    metasnf_error(
        "The merge function is not applicable to `solutions_df` class",
        " objects. To combine two `solutions_df` objects, use `rbind()`."
    )
}

#' Merge `t_ext_solutions_df` objects
#'
#' @param x The first `t_ext_solutions_df` object to merge.
#' @param y The second `t_ext_solutions_df` object to merge.
#' @param ... Additional arguments (not used).
#' @return Error message indicating that the merge function is not applicable
#' to `t_ext_solutions_df` objects.
#' @export
merge.t_ext_solutions_df <- function(x, y, ...) {
    metasnf_error(
        "The merge function is not applicable to `t_ext_solutions_df` class",
        " objects. To combine two `t_ext_solutions_df` objects, use `rbind()`."
    )
}

#' Merge `t_solutions_df` objects
#'
#' @param x The first `t_solutions_df` object to merge.
#' @param y The second `t_solutions_df` object to merge.
#' @param ... Additional arguments (not used).
#' @return Error message indicating that the merge function is not applicable
#' to `t_solutions_df` objects.
#' @export
merge.t_solutions_df <- function(x, y, ...) {
    metasnf_error(
        "The merge function is not applicable to `t_solutions_df` class",
        " objects. To combine two `t_solutions_df` objects, use `rbind()`."
    )
}

#' Merge `weights_matrix` objects
#'
#' @param x The first `weights_matrix` object to merge.
#' @param y The second `weights_matrix` object to merge.
#' @param ... Additional arguments (not used).
#' @return Error message indicating that the merge function is not applicable
#' to `weights_matrix` objects.
#' @export
merge.weights_matrix <- function(x, y, ...) {
    metasnf_error(
        "The merge function is not applicable to `weights_matrix` class",
        " objects. To combine two `weights_matrix` objects, use `rbind()`."
    )
}

