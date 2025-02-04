#' @export
`[<-.data_list` <- function(x, i, value) {
    dll <- NextMethod()
    validate_data_list(dll)
    dl <- as_data_list(dll)
    return(dl)
}

#' @export
`[<-.settings_df` <- function(x, i, j, value) {
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
`[<-.solutions_df` <- function(x, i, j, value) {
    result <- NextMethod()
    class(result) <- setdiff(class(result), "solutions_df")
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
`[<-.ext_solutions_df` <- function(x, i, j, value) {
    result <- NextMethod()
    class(result) <- setdiff(class(result), "ext_solutions_df")
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
`[<-.weights_matrix` <- function(x, i, j, value) {
    result <- NextMethod()
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
`[<-.ari_matrix` <- function(x, i, j, value) {
    result <- NextMethod()
    class(result) <- setdiff(class(result), "ari_matrix")
    result <- tryCatch(
        expr = {
            result <- validate_ari_matrix(result)
            result <- new_ari_matrix(result)
            result
        },
        error = function(e) {
            result
        }
    )
    return(result)
}

#' @export
`[<-.sim_mats_list` <- function(x, i, value) {
    cat("cake")
    NextMethod()
}
