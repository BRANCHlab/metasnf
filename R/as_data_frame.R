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

#' Coerce a `ext_solutions_df` class object into a `data.frame` class object
#'
#' @param x A `ext_solutions_df` class object.
#' @param row.names Additional parameter passed to `as.data.frame()`.
#' @param optional Additional parameter passed to `as.data.frame()`.
#' @param ... Additional parameter passed to `as.data.frame()`.
#' @return A `data.frame` class object with all the columns of x and its
#'  contained solutions data frame.
#' @export
as.data.frame.ext_solutions_df <- function(x,
                                           row.names = NULL,
                                           optional = FALSE,
                                           ...) {
    df <- dplyr::inner_join(
        attributes(x)$"solutions_df",
        x,
        by = "solution"
    ) |>
        data.frame()
    return(df)
}

