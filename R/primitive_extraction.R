#' Extraction operator for data lists
#'
#' Enables usage of `[` extraction operator on `data_list` class objects. Given
#' a `numeric` or `character` vector of indices, returns a subsetted data list.
#' Data lists only use single-dimension extraction (component-wise).
#'
#' @param x A `data_list` class object.
#' @param i Indices for component extraction.
#' @param ... Additional parameters (invalid for data list extraction).
#' @return `data_list` class object of extracted components.
#' @export
`[.data_list` <- function(x, i, ...) {
    extra_args <- list(...)
    if (length(extra_args) > 0) {
       metasnf_error(
           "Incorrect number of dimensions for data list subsetting."
       )
    }
    class(x) <- "list"
    dll <- NextMethod()
    validate_data_list(dll)
    dl <- as_data_list(dll)
    return(dl)
}
