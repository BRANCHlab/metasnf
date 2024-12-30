#' Validator for `snf_results` class object
#'
#' @keywords internal
#' @param srl A snf_results-like list object to be validated.
#' @return If srl has a valid structure for a `snf_results` class
#'  object, returns the input unchanged. Otherwise, raises an error.
validate_snf_results <- function(srl) {
    class(srl)  <- setdiff(class(srl), "snf_results")
    return(srl)
}

#' Constructor for `snf_results` class object
#' 
#' @keywords internal
#' @inheritParams validate_snf_results
#' @return An `snf_results` object.
new_snf_results <- function(srl) {
    sr <- structure(srl, class = c("snf_results", "list"))
}
