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
