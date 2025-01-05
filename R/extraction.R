#' Extraction operator for data lists
#'
#' Enables usage of `[` extraction operator on `data_list` class objects. Given
#' a `numeric` or `character` vector of indices, returns a subsetted data list.
#' Data lists only use single-dimension extraction (component-wise).
#'
#' @param x A `data_list` class object.
#' @param i Indices for component extraction.
#' @param ... Additional parameters (invalid for data list extraction).
#' @return `data_list` class object of extracted components.
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

#' Extraction operator for settings data frames
#'
#' Enables usage of `[` extraction operator on `settings_df` class objects.
#'
#' @param x A `settings_df` class object.
#' @param i Indices for component extraction.
#' @param j Indices for component extraction.
#' @param ... Additional parameters (invalid for settings df extraction).
#' @return `settings_df` class object of extracted components.
#' @export
`[.settings_df` <- function(x, i, j, ...) {
    #extra_args <- list(...)
    #if (length(extra_args) > 0) {
    #   metasnf_error(
    #       "Incorrect number of dimensions for settings df subsetting."
    #   )
    #}
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

#' Extraction operator for SNF config
#'
#' Enables usage of `[` extraction operator on `snf_config` class objects.
#'
#' @param x A `snf_config` class object.
#' @param i Indices for component extraction.
#' @param ... Additional parameters (invalid for snf_config extraction).
#' @return `snf_config` class object of extracted components.
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

#' Extraction operator for weights matrix
#'
#' Enables usage of `[` extraction operator on `weights_matrix` class objects.
#'
#' @param x A `weights_matrix` class object.
#' @param i Indices for component extraction.
#' @param j Indices for component extraction.
#' @param ... Additional parameters (invalid for weights_matrix extraction).
#' @return `weights_matrix` class object of extracted components.
#' @export
`[.weights_matrix` <- function(x, i, j, ...) {
    result <- NextMethod("[")
    class(result) <- class(x)
    result
}

#' Extraction operator for weights matrix
#'
#' Enables usage of `[` extraction operator on `weights_matrix` class objects.
#'
#' @param x A `weights_matrix` class object.
#' @param i Indices for component extraction.
#' @param j Indices for component extraction.
#' @param ... Additional parameters (invalid for weights_matrix extraction).
#' @return `weights_matrix` class object of extracted components.
#' @export
`[.solutions_df` <- function(x, i, ...) {
    extra_args <- list(...)
    if (length(extra_args) > 0) {
        metasnf_error(
            "Incorrect number of dimensions for subsetting solutions_df."
        )
    }
    browser()
    result <- NextMethod("[", , i, ...)
    class(result) <- class(x)
    attributes(result) <- attributes(x)
    attributes(result)$"snf_config" <- attributes(result)$"snf_config"[i]
    result
}
