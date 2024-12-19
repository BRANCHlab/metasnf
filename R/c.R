#' Combine an arbitrary number of data lists into one data list
#'
#' Horizontally joins data frames within a data list into a single data frame,
#' using the `uid` attribute as the joining key.
#'
#' @param ... Data lists to be concatenated.
#' @return A data list made of the concatenated inputs.
#' @export
c.data_list <- function(...) {
    dls <- list(...)
    dlls <- lapply(
        dls,
        function(x) {
            class(x) <- "list"
            return(x)
        }
    )
    dll <- do.call(c, dlls) |>
        reduce_dll_to_common() |>
        arrange_dll() |>
        dll_uid_first_col()
    validate_data_list(dll)
    dl <- new_data_list(dll)
    return(dl)
}
