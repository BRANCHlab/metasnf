#' Convert an object to an ARI matrix
#'
#' This function coerces non-`ari_matrix` class objects into
#' `ari_matrix` class objects.
#'
#' @param x The object to convert into a weights matrix.
#' @return An `ari_matrix` class object.
#' @export
as_ari_matrix <- function(x) {
    UseMethod("as_ari_matrix")
}

#' @export
as_ari_matrix.matrix <- function(x) {
    aml <- validate_ari_matrix(x)
    am <- new_ari_matrix(aml)
    return(am)
}
