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
summarize_target_list <- function(target_list) {
    message(
        "`summarize_target_list` has been deprecated. Please use",
        " `summarize_dl` instead."
    )
}
