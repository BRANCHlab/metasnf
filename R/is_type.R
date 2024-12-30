#' Check if an object is properly formatted
#'
#' This function coerces non-`data_list` class objects into `data_list` class
#' objects.
#'
#' @param x The object to check for validity
#' @param validator_fn Helper function containing tryCatch statement
#' @return TRUE if the object is a valid member of its main class, FALSE
#'  otherwise.
#' @export
is_valid <- function(x, validator_fn) {
    UseMethod("is_valid")
}

#' Helper function for is_valid tryCatch statement
#'
#' @keywords internal
#' @inheritParams is_valid
#' @return RETURN
validate_try_catch <- function(x, validator_fn) {
    tryCatch(
        {
            validator_fn(x)
            return(TRUE)
        },
        error = function(e) {
            return(FALSE)
        }
    )
}

#' @export
is_valid.settings_df <- function(x, validator_fn) {
    validate_try_catch(x, validator_fn)
}

