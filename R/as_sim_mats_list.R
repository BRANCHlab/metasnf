#' Convert an object to a similarity matrix list
#'
#' This function converts non-`sim_mats_list` class objects into
#' `sim_mats_list` class objects.
#'
#' @param x The object to convert into a `sim_mats_list`. Must be a list of
#'  square matrices with identical column and row names.
#' @return A `sim_mats_list` class object.
#' @export
as_sim_mats_list <- function(x) {
    UseMethod("as_sim_mats_list")
}

#' @export
as_sim_mats_list.list <- function(x) {
    x <- validate_sim_mats_list(x)
    x <- new_sim_mats_list(x)
    return(x)
}
