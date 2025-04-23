#' Pull UIDs from an object
#'
#' @param x The object to extract UIDs from.
#' @return A character vector of UIDs.
#' @export
uids <- function(x) {
    UseMethod("uids")
}

#' @export
uids.data_list <- function(x) {
    uid_vec <- attributes(x)$"uids"
    return(uid_vec)
}

#' @export
uids.default <- function(x) {
    uid_vec <- attributes(x)$"uids"
    if (is.null(uid_vec)) {
        metasnf_warning(
            "No UIDs in object of type: ", class(x)[[1]], "\n"
        )
    } else {
        return(uid_vec)
    }
}

#' @export
uids.ext_solutions_df <- function(x) {
    uid_vec <- colnames(x)[grepl("^uid_", colnames(x))]
    return(uid_vec)
}

#' @export
uids.sim_mats_list <- function(x) {
    if (length(x) >= 1) {
        uid_vec <- colnames(x[[1]])
        return(uid_vec)
    } else {
        metasnf_warning(
            "No UIDs in empty `sim_mats_list`."
        )
    }
}

#' @export
uids.solutions_df <- function(x) {
    uid_vec <- colnames(x)[grepl("^uid_", colnames(x))]
    return(uid_vec)
}

#' @export
uids.t_solutions_df <- function(x) {
    return(x$"uid")
}

#' @export
uids.t_ext_solutions_df <- function(x) {
    uids(t(x))
}
