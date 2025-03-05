#' Coerce a `weights_matrix` class object into a `matrix` class object
#'
#' @param x A `weights_matrix` class object.
#' @param ... Additional parameter passed to `as.matrix()`.
#' @return A `matrix` and `array` class object.
#' @export
as.matrix.weights_matrix <- function(x,
                                     ...) {
    class(x) <- c("matrix", "array")
    return(x)
}
