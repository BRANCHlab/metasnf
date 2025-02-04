#' Convert an object to a snf config
#'
#' This function coerces non-`snf_config` class objects into `snf_config` class
#' objects.
#'
#' @param x The object to convert into a snf config.
#' @return A `snf_config` class object.
#' @export
as_snf_config <- function(x) {
    UseMethod("as_snf_config")
}

#' @export
as_snf_config.solutions_df <- function(x) {
    return(attributes(x)$"snf_config")
}

#' @export
as_snf_config.ext_solutions_df <- function(x) {
    return(attributes(x)$"snf_config")
}
