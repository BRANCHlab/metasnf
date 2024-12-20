#' Generate a matrix to store feature weights
#'
#' @param dl A nested list of input data from `data_list()`.
#' @param n_solutions Number of rows to generate the template weights matrix for.
#' @param weights_fill String indicating what to populate generate rows with.
#'  Can be "ones" (default; fill matrix with 1), "uniform" (fill matrix with
#'  uniformly distributed random values), or "exponential" (fill matrix with
#'  exponentially distributed random values).
#' @return wm A properly formatted matrix containing columns for
#'  all the features that require weights and rows.
#' @export
weights_matrix <- function(dl = NULL,
                           n_solutions = 1,
                           weights_fill = "ones") {
    if (is.null(dl)) {
        metasnf_error(
            "Constructing a weights matrix using `weights_matrix()` requires ",
            "feature information from a data list. Alternatively, a pre-made",
            " matrix can be passed into `as_weights_matrix()`."
        )
    }
    if (is.null(n_solutions)) {
        metasnf_error("`n_solutions` must be specified.")
    }
    features <- attributes(dl)$"features"
    if (weights_fill == "ones") {
        weights_fill <- 1
    } else if (weights_fill == "uniform") {
        weights_fill <- stats::runif(n_solutions * length(features))
    } else if (weights_fill == "exponential") {
        weights_fill <- stats::rexp(n_solutions * length(features))
    }
    wml <- matrix(
        nrow = n_solutions,
        ncol = length(features),
        data = weights_fill
    )
    colnames(wml) <- features
    wml <- validate_weights_matrix(wml)
    wm <- new_weights_matrix(wml)
    return(wm)
}

#' Validator for `weights_matrix` class object
#'
#' @keywords internal
#' @param wml A weights_matrix-like matrix object to be validated.
#' @return If wml has a valid structure for a `weights_matrix` class
#'  object, returns the input unchanged. Otherwise, raises an error.
validate_weights_matrix <- function(wml) {
    # 1. Ensure is matrix
    if (!inherits(wml, "matrix")) {
        metasnf_error("`weights_matrix` must inherit from class `matrix`.")
    }
    # 2. Ensure is array
    if (!inherits(wml, "array")) {
        metasnf_error("`weights_matrix` must inherit from class `array`.")
    }
    # 3. Check for NAs
    if (any(is.na(wml))) {
        metasnf_error("Weights matrix cannot have missing values.")
    }
    return(wml)
}

#' Constructor for `new_weights_matrix` class object
#' 
#' @keywords internal
#' @inheritParams validate_weights_matrix
#' @return A `new_weights_matrix` object.
new_weights_matrix <- function(wml) {
    wm <- structure(wml, class = c("weights_matrix", "matrix", "array"))    
    return(wm)
}

#' Convert an object to a weights matrix
#'
#' This function coerces non-`weights_matrix` class objects into
#' `weights_matrix` class objects.
#'
#' @param x The object to convert into a weights matrix.
#' @return A `weights_matrix` class object.
#' @export
as_weights_matrix <- function(x) {
    UseMethod("as_weights_matrix")
}

#' @export
as_weights_matrix.matrix <- function(x) {
    validate_weights_matrix(x)
    wm <- new_weights_matrix(x)
    return(wm)
}
