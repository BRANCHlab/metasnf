#' Convert an object to a data list
#'
#' This function coerces non-`data_list` class objects into `data_list` class
#' objects.
#'
#' @param x The object to convert into a data list.
#' @return A `data_list` class object.
#' @export
as_data_list <- function(x) {
    UseMethod("as_data_list")
}

#' @export
as_data_list.list <- function(x) {
    validate_data_list(x)
    dl <- new_data_list(x)
    return(dl)
}

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

