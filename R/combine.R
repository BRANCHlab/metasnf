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
    combined_dll <- do.call(c, dlls)
    validate_data_list(combined_dll)
    dl <- new_data_list(combined_dll)
    return(dl)
}

