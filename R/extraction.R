#' @export
`[.clust_fns_list` <- function(x, i, ...) {
    extra_args <- list(...)
    if (length(extra_args) > 0) {
        metasnf_error(
            "Incorrect number of dimensions for clust_fns_list subsetting."
        )
    }
    result <- NextMethod()
    class(result) <- c("clust_fns_list", "list")
    result
}

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
`[.dist_fns_list` <- function(x, i, ...) {
    result <- NextMethod()
    class(result) <- "list"
    result <- tryCatch(
        expr = {
            result <- validate_dist_fns_list(result)
            result <- new_dist_fns_list(result)
            result
        },
        error = function(e) {
            result
        }
    )
    return(result)
}

#' @export
`[.settings_df` <- function(x, i, j, ...) {
    if (missing(j) & nargs() == 2) {
        return(x[i, ])
    } else {
        result <- NextMethod()
    }
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
    if (!missing(i)) {
        x$"settings_df" <- x$"settings_df"[i, , drop = FALSE]
        x$"weights_matrix" <- x$"weights_matrix"[i, , drop = FALSE]
    }
    attr(x, "n_solutions") <- nrow(x$"settings_df")
    class(x) <- setdiff(class(x), "snf_config")
    x <- tryCatch(
        expr = {
            x <- validate_snf_config(x)
            x <- new_snf_config(x)
            return(x)
        },
        error = function(e) {
            return(x)
        }
    )
    return(x)
}

#' @export
`[.weights_matrix` <- function(x, i, j, ...) {
    result <- NextMethod("[")
    class(result) <- setdiff(class(result), "weights_matrix")
    result <- tryCatch(
        expr = {
            result <- validate_weights_matrix(result)
            result <- new_weights_matrix(result)
            result
        },
        error = function(e) {
            result
        }
    )
    return(result)
}

#' @export
`[.solutions_df` <- function(x, i, j, ...) {
    result <- NextMethod()
    class(result) <- setdiff(class(result), "solutions_df")
    if (nargs() == 2 && !missing(i)) {
        attr(result, "sim_mats_list") <- attr(x, "sim_mats_list")
        attr(result, "snf_config") <- attr(x, "snf_config")
    } else if (!missing(i)) {
        attr(result, "sim_mats_list") <- attr(x, "sim_mats_list")[i]
        attr(result, "snf_config") <- attr(x, "snf_config")[i]
    } else {
        attr(result, "sim_mats_list") <- attr(x, "sim_mats_list")
        attr(result, "snf_config") <- attr(x, "snf_config")
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
    if (nargs() == 2 && !missing(i)) {
        attr(result, "sim_mats_list") <- attr(x, "sim_mats_list")
        attr(result, "snf_config") <- attr(x, "snf_config")
    } else if (!missing(i)) {
        attr(result, "sim_mats_list") <- attr(x, "sim_mats_list")[i]
        attr(result, "snf_config") <- attr(x, "snf_config")[i]
    } else {
        attr(result, "sim_mats_list") <- attr(x, "sim_mats_list")
        attr(result, "snf_config") <- attr(x, "snf_config")
    }
    if (!is.null(attr(x, "summary_features"))) {
        attr(result, "summary_features") <- attr(x, "summary_features")
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
