#' Extract number of features stored in an object
#'
#' @param x The object to extract number of features from.
#' @return The number of features in x.
#' @export
n_features <- function(x) {
    UseMethod("n_features")
}

#' @export
n_features.data_list <- function(x) {
    return(attributes(x)$"n_features")
}
