#' @export
`[.data_list` <- function(x, i, ...) {
    extra_args <- list(...)
    if (length(extra_args) > 0) {
        metasnf_error(
            "Incorrect number of dimensions for data list subsetting."
        )
    }
    class(x) <- "list"
    dll <- NextMethod()
    validate_data_list(dll)
    dl <- as_data_list(dll)
    return(dl)
}

#' @export
`[.settings_df` <- function(x, i, j, ...) {
    result <- NextMethod()
    class(result) <- setdiff(class(result), "settings_df")
    result <- tryCatch(
        expr = {
            result <- validate_settings_df(result)
            result <- new_settings_df(result)
            result
        },
        error = function(e) {
            result
        }
    )
    return(result)
}

#' @export
`[.snf_config` <- function(x, i, ...) {
    extra_args <- list(...)
    if (length(extra_args) > 0) {
        metasnf_error(
            "Incorrect number of dimensions for SNF config subsetting."
        )
    }
    x$"settings_df" <- x$"settings_df"[i, , drop = FALSE]
    x$"weights_matrix" <- x$"weights_matrix"[i, , drop = FALSE]
    x
}

#' @export
`[.weights_matrix` <- function(x, i, j, ...) {
    result <- NextMethod("[")
    class(result) <- class(x)
    result
}

#' @export
`[.solutions_df` <- function(x, i, j, ...) {
    result <- NextMethod()
    class(result) <- setdiff(class(result), "solutions_df")
    if (!missing(i)) {
        attributes(result)$"sim_mats_list" <- attributes(x)$"sim_mats_list"[i]
        attributes(result)$"snf_config" <- attributes(x)$"snf_config"[i]
    } else {
        attributes(result)$"sim_mats_list" <- attributes(x)$"sim_mats_list"
        attributes(result)$"snf_config" <- attributes(x)$"snf_config"
    }
    result <- tryCatch(
        expr = {
            result <- validate_solutions_df(result)
            result <- new_solutions_df(result)
            result
        },
        error = function(e) {
            result
        }
    )
    return(result)
}

#' @export
`[.ext_solutions_df` <- function(x, i, j, ...) {
    result <- NextMethod()
    class(result) <- setdiff(class(result), "ext_solutions_df")
    if (!missing(i)) {
        attributes(result)$"sim_mats_list" <- attributes(x)$"sim_mats_list"[i]
        attributes(result)$"snf_config" <- attributes(x)$"snf_config"[i]
    } else {
        attributes(result)$"sim_mats_list" <- attributes(x)$"sim_mats_list"
        attributes(result)$"snf_config" <- attributes(x)$"snf_config"
    }
    result <- tryCatch(
        expr = {
            result <- validate_ext_solutions_df(result)
            result <- new_ext_solutions_df(result)
            result
        },
        error = function(e) {
            result
        }
    )
    return(result)
}

#' @export
`[.sim_mats_list` <- function(x, i, ...) {
    smll <- NextMethod()
    smll <- validate_sim_mats_list(smll)
    sml <- as_sim_mats_list(smll)
    return(sml)
}
