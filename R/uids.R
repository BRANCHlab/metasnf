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
    uid_vec <- colnames(x)[-c(1:2)]
    return(uid_vec)
}

#' @export
uids.data_list <- function(x) {
    uid_vec <- attributes(x)$"uids"
    return(uid_vec)
}

#' @export
uids.ext_solutions_df <- function(x) {
    uid_vec <- uids(attributes(x)$"solutions_df")
    return(uid_vec)
}
