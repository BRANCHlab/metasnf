#' Pull features used to calculate summary p-values from an object
#'
#' @keywords internal
#' @param x The object to extract summary features from.
#' @return A character vector of summary features.
#' @export
summary_features <- function(x) {
    UseMethod("summary_features")
}

#' @keywords internal
#' @export
summary_features.ext_solutions_df <- function(x) {
    summary_fts <- attributes(x)$"summary_features"
    return(summary_fts)
}
