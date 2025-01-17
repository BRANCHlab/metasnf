#' Return character vector of features stored in an object
#'
#' @param x The object to pull features from.
#' @return A character vector of features in x.
#' @export
features <- function(x) {
    UseMethod("features")
}

#' @export
features.data_list <- function(x) {
    return(attributes(x)$"features")
}

#' @export
features.solutions_df <- function(x) {
    return(attributes(x)$"features")
}

#' @export
features.ext_solutions_df <- function(x) {
    return(attributes(x)$"features")
}
