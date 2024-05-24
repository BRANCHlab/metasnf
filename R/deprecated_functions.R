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
