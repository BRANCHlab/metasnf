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
    new_wm <- rbind(x$"weights_matrix", y$"weights_matrix")
    x$"settings_df" <- new_sdf
    x$"weights_matrix" <- new_wm
    if (reset_indices) {
        x$"settings_df"$"solution" <- seq_len(nrow(x$"settings_df"))
    }
    x <- validate_snf_config(x)
    x <- new_snf_config(x)
    return(x)
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
    dl_1 <- x
    dl_2 <- y
    dl_1_names <- summary(dl_1)$"name"
    dl_2_names <- summary(dl_2)$"name"
    names(dl_1) <- dl_1_names
    names(dl_2) <- dl_2_names
    if (!identical(sort(dl_1_names), sort(dl_2_names))) {
        metasnf_error(
            "The two data lists must have identical components."
        )
    }
    merged_data_list <- lapply(
        dl_1_names,
        function(x) {
            dl_1[[x]]$"data" <- dplyr::bind_rows(
                dl_1[[x]]$"data",
                dl_2[[x]]$"data"
            )
            dl_1[[x]]$"data" <- dplyr::arrange(
                dl_1[[x]]$"data", 
                dl_1[[x]]$"data"$"uid"
            )
            return(dl_1[[x]])
        }
    )
    names(merged_data_list) <- dl_1_names
    merged_data_list <- as_data_list(merged_data_list)
    return(merged_data_list)
}

