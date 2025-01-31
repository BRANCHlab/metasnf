#' Select variables from an object
#'
#' @param .data The object to select columns from.
#' @param ... Additional arguments for selection.
#' @return Object with selected columns.
#' @export
select <- function(.data, ...) {
    UseMethod("select")
}

#' @export
select.solutions_df <- function(.data, ...) {
    metasnf_error("`select` cannot be used on `solutions_df` class objects.")
}

#' @export
select.ext_solutions_df <- function(.data, ...) {
    metasnf_error(
        "`select` cannot be used on `ext_solutions_df` class objects."
    )
}

#' @export
select.t_solutions_df <- function(.data, ...) {
    metasnf_error(
        "`select` cannot be used on `t_solutions_df` class objects."
    )
}

#' @export
select.t_ext_solutions_df <- function(.data, ...) {
    metasnf_error(
        "`select` cannot be used on `t_ext_solutions_df` class objects."
    )
}

#' @export
select.settings_df <- function(.data, ...) {
    metasnf_error(
        "`select` cannot be used on `settings_df` class objects."
    )
}

#' @export
select.snf_config <- function(.data, ...) {
    metasnf_error(
        "`select` cannot be used on `snf_config` class objects."
    )
}

#' @export
select.ari_matrix <- function(.data, ...) {
    metasnf_error(
        "`select` cannot be used on `weights_matrix` class objects."
    )
}
