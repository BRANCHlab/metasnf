#' Convert an object to a weights matrix
#'
#' This function converts non-`weights_matrix` objects into `weights_matrix`
#' class objects.
#'
#' @param x The object to convert into a data list.
#' @return A `weights_matrix` class object.
#' @export
as_weights_matrix <- function(x) {
    UseMethod("as_weights_matrix")
}

#' @export
as_weights_matrix.matrix <- function(x) {
    x <- validate_weights_matrix(x)
    x <- new_weights_matrix(x)
    return(x)
}
