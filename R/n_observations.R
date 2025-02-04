#' Extract number of observations stored in an object
#'
#' @param x The object to extract number of observations from.
#' @return The number of observations in x.
#' @export
n_observations <- function(x) {
    UseMethod("n_observations")
}

#' @export
n_observations.data_list <- function(x) {
    return(attributes(x)$"n_observations")
}
