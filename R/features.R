#' Return character vector of features stored in an object
#'
#' @keywords internal
#' @param x The object to pull features from.
#' @return A character vector of features in x.
#' @export
features <- function(x) {
    UseMethod("features")
}

#' @keywords internal
#' @export
features.data_list <- function(x) {
    return(attributes(x)$"features")
}

#' @keywords internal
#' @export
features.solutions_df <- function(x) {
    return(attributes(x)$"features")
}

#' @keywords internal
#' @export
features.ext_solutions_df <- function(x) {
    return(attributes(x)$"features")
}
