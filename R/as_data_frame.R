#' Coerce a `data_list` class object into a `data.frame` class object
#'
#' Horizontally joins data frames within a data list into a single data frame,
#' using the `uid` attribute as the joining key.
#'
#' @param x A `data_list` class object.
#' @param row.names Additional parameter passed to `as.data.frame()`.
#' @param optional Additional parameter passed to `as.data.frame()`.
#' @param ... Additional parameter passed to `as.data.frame()`.
#' @return dl_df A `data.frame` class object with all the features and
#'  observations of `dl`.
#' @export
as.data.frame.data_list <- function(x,
                                    row.names = NULL,
                                    optional = FALSE,
                                    ...) {
    data_only <- x |> lapply(
        function(component) {
            return(component$"data")
        }
    )
    dl_df <- merge_df_list(data_only) |>
        as.data.frame(
            row.names = row.names,
            optional = optional,
            ... = ...
        )
    return(dl_df)
}

#' Coerce a `solutions_df` class object into a `data.frame` class object
#'
#' @param x A `solutions_df` class object.
#' @param row.names Additional parameter passed to `as.data.frame()`.
#' @param optional Additional parameter passed to `as.data.frame()`.
#' @param keep_attributes If TRUE, resulting data frame includes settings
#'  data frame and weights matrix.
#' @param ... Additional parameter passed to `as.data.frame()`.
#' @return A `data.frame` class object with all the columns of x and its
#'  contained solutions data frame.
#' @export
as.data.frame.solutions_df <- function(x,
                                       row.names = NULL,
                                       optional = FALSE,
                                       keep_attributes = FALSE,
                                       ...) {
    if (keep_attributes) {
        sdf <- attributes(x)$"snf_config"$"settings_df"
        wm <- attributes(x)$"snf_config"$"weights_matrix"
        sdf_wm <- cbind(data.frame(sdf), data.frame(wm))
        df <- dplyr::inner_join(
            sdf_wm,
            x,
            by = "solution"
        )
    } else {
        df <- NextMethod()
    }
    return(df)
}

#' Coerce a `ext_solutions_df` class object into a `data.frame` class object
#'
#' @param x A `ext_solutions_df` class object.
#' @param row.names Additional parameter passed to `as.data.frame()`.
#' @param optional Additional parameter passed to `as.data.frame()`.
#' @param keep_attributes If TRUE, resulting data frame includes settings
#'  data frame and weights matrix.
#' @param ... Additional parameter passed to `as.data.frame()`.
#' @return A `data.frame` class object with all the columns of x and its
#'  contained solutions data frame.
#' @export
as.data.frame.ext_solutions_df <- function(x,
                                           row.names = NULL,
                                           optional = FALSE,
                                           keep_attributes = FALSE,
                                           ...) {
    if (keep_attributes) {
        sdf <- attributes(x)$"snf_config"$"settings_df"
        wm <- attributes(x)$"snf_config"$"weights_matrix"
        sdf_wm <- cbind(data.frame(sdf), data.frame(wm))
        sdf_wm$"solution" <- as.numeric(sdf_wm$"solution")
        x$"solution" <- as.numeric(x$"solution")
        df <- dplyr::inner_join(
            sdf_wm,
            x,
            by = "solution"
        )
    } else {
        df <- NextMethod()
    }
    return(df)
}
