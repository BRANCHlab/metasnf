#' Convert a data list into a data frame
#'
#' @description
#' `r lifecycle::badge("defunct")`
#' Defunct function for converting a data list into a data frame. Please
#'  use `as.data.frame()` instead.
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#' @return A "data.frame"-formatted version of the provided data list.
#' @export
collapse_dl <- function(data_list) {
    metasnf_defunct(
        "2.0.0",
        paste0(
            "Please ensure your data list is a `data_list` class object made ",
            "by `data_list()` rather than `generate_data_list()` and then cal",
            "l `as.data.frame` instead."
        )
    )
}
