#' Select method for class `settings_df`
#'
#' Reduce a settings data frame to a subset of its columns.
#'
#' @param .data A `settings_df` class object.
#' @param ... Other arguments passed to `select` (not used in this function)
#' @return If the result of the selection is a valid settings data frame,
#'  returns that result. Otherwise, returns that result as a non-settings_df.
#' @export
select.settings_df <- function(.data, ...) {
    result <- NextMethod()
    if (!is_valid(result)) {
        class(result) <- setdiff(class(result), "settings_df")
        print("cat")
    } else {
        print("dog")
    }
    return(result)
}
