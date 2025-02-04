#' Convert an object to a data list
#'
#' This function coerces non-`data_list` class objects into `data_list` class
#' objects.
#'
#' @param x The object to convert into a data list.
#' @return A `data_list` class object.
#' @export
as_data_list <- function(x) {
    UseMethod("as_data_list")
}

#' @export
as_data_list.list <- function(x) {
    validate_data_list(x)
    dl <- new_data_list(x)
    return(dl)
}
