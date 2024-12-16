#' Collapse a dl into a single dataframe
#' 
#' @rdname deprecated
#' @param dl A nested list of input data from `data_list()`.
#' @return A "data.frame"-formatted version of the provided data list.
#' @export
collapse_dl <- function(dl) {
    metasnf_deprecated("2.0.0", "use `as.data.frame()` instead.")
}
