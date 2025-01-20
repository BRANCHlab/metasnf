#' Pull UIDs from an object
#'
#' @param x The object to extract UIDs from.
#' @return A character vector of UIDs.
#' @export
uids <- function(x) {
    UseMethod("uids")
}

#' @export
uids.solutions_df <- function(x) {
    uid_vec <- x |>
        dplyr::select(dplyr::starts_with("uid_")) |>
        colnames()
    return(uid_vec)
}

#' @export
uids.t_solutions_df <- function(x) {
    return(x$"uid")
}

#' @export
uids.data_list <- function(x) {
    uid_vec <- attributes(x)$"uids"
    return(uid_vec)
}

#' @export
uids.ext_solutions_df <- function(x) {
    uid_vec <- x |>
        dplyr::select(dplyr::starts_with("uid_")) |>
        colnames()
    return(uid_vec)
}
