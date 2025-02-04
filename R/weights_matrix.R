#' Generate a matrix to store feature weights
#'
#' Function for building a weights matrix independently of an SNF config. The
#' weights matrix contains one row corresponding to each row of the settings
#' data frame in an SNF config (one row for each resulting cluster solution)
#' and one column for each feature in the data list used for clustering. Values
#' of the weights matrix are passed to distance metrics functions during the
#' conversion of input data frames to distance matrices. Typically, there is no
#' need to use this function directly. Instead, users should provide weights
#' matrix-building parameters to the `snf_config()` function.
#'
#' @param dl A nested list of input data from `data_list()`.
#' @param n_solutions Number of rows to generate the template weights matrix
#'  for.
#' @param weights_fill String indicating what to populate generate rows with.
#'  Can be "ones" (default; fill matrix with 1), "uniform" (fill matrix with
#'  uniformly distributed random values), or "exponential" (fill matrix with
#'  exponentially distributed random values).
#' @return wm A properly formatted matrix containing columns for
#'  all the features that require weights and rows.
#' @export
#' @examples
#' input_dl <- data_list(
#'     list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
#'     list(income, "income", "demographics", "continuous"),
#'     list(pubertal, "pubertal_status", "demographics", "continuous"),
#'     uid = "unique_id"
#' )
#' 
#' sc <- snf_config(input_dl, n_solutions = 5)
#' 
#' wm <- weights_matrix(input_dl, n_solutions = 5, weights_fill = "uniform")
#' 
#' # updating an SNF config in parts
#' sc$"weights_matrix" <- wm
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
    fts <- features(dl)
    n_fts <- n_features(dl)
    if (weights_fill == "ones") {
        weights_fill <- 1
    } else if (weights_fill == "uniform") {
        weights_fill <- stats::runif(n_solutions * n_fts)
    } else if (weights_fill == "exponential") {
        weights_fill <- stats::rexp(n_solutions * n_fts)
    }
    wml <- matrix(
        nrow = n_solutions,
        ncol = n_fts,
        data = weights_fill
    )
    colnames(wml) <- fts
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
    class(wml) <- setdiff(class(wml), "weights_matrix")
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

#' Constructor for `weights_matrix` class object
#' 
#' @keywords internal
#' @inheritParams validate_weights_matrix
#' @return A `weights_matrix` object.
new_weights_matrix <- function(wml) {
    wm <- structure(wml, class = c("weights_matrix", "matrix", "array"))    
    return(wm)
}
