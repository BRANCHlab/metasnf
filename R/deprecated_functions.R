#' (DEPRECATED) Generate target_list object
#'
#' @param ... Function arguments
#'
#' @export
generate_target_list <- function(...) {
    message(
        "`generate_target_list` has been deprecated. Please use",
        " `generate_data_list` to build your target lists instead."
    )
}

#' (DEPRECATED) Summarize target list
#'
#' @param ... Function arguments
#'
#' @export
summarize_target_list <- function(...) {
    message(
        "`summarize_target_list` has been deprecated. Please use",
        " `summarize_dl` instead."
    )
}

#' (DEPRECATED) Get p-value
#'
#' @param ... Function arguments
#'
#' @export
get_cluster_pval <- function(...) {
    message(
        "`get_cluster_pval` has been deprecated. Please use `calc_assoc_pval`."
    )
}

#' (DEPRECATED) Manhattan plot creation
#'
#' @param ... Function arguments
#'
#' @export
manhattan_plot <- function(...) {
    message(
        "`manhattan_plot` has been deprecated. Please check one of the new",
        " functions, `esm_manhattan_plot`, `var_manhattan_plot`, or",
        " `mc_manhattan_plot` to achieve similar functionality."
    )
}

#' (DEPRECATED) Association p-value calculations
#'
#' @param ... Function arguments
#'
#' @export
calculate_associations <- function(...) {
    message(
        "`calculate_associations` has been deprecated. Please use",
        " `calc_assoc_pval` instead."
    )
}

#' (DEPRECATED) Association p-value heatmap
#'
#' @param ... Function arguments
#'
#' @export
association_heatmap <- function(...) {
    message(
        "`association_heatmap` has been deprecated. Please use",
        " `assoc_pval_heatmap` instead."
    )
}
