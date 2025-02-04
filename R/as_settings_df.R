#' Convert an object to a settings data frame
#'
#' This function coerces non-`settings_df` class objects into `settings_df` class
#' objects.
#'
#' @param x The object to convert into a data list.
#' @return A `settings_df` class object.
#' @export
as_settings_df <- function(x) {
    UseMethod("as_settings_df")
}

#' @export
as_settings_df.data.frame <- function(x) {
    rownames(x) <- as.character(rownames(x))
    validate_settings_df(x)
    sdf <- new_settings_df(x)
    return(sdf)
}
