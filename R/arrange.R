#' Arrange rows in an object
#'
#' @param .data The object to arrange columns from.
#' @param ... Additional arguments for arranging.
#' @return Object with arrangeed columns.
#' @export
arrange <- function(.data, ...) {
    UseMethod("arrange")
}

#' @export
arrange.solutions_df <- function(.data, ...) {
    metasnf_error("`arrange` cannot be used on `solutions_df` class objects.")
}

#' @export
arrange.ext_solutions_df <- function(.data, ...) {
    metasnf_error(
        "`arrange` cannot be used on `ext_solutions_df` class objects."
    )
}

#' @export
arrange.t_solutions_df <- function(.data, ...) {
    metasnf_error(
        "`arrange` cannot be used on `t_solutions_df` class objects."
    )
}

#' @export
arrange.t_ext_solutions_df <- function(.data, ...) {
    metasnf_error(
        "`arrange` cannot be used on `t_ext_solutions_df` class objects."
    )
}

#' @export
arrange.settings_df <- function(.data, ...) {
    metasnf_error(
        "`arrange` cannot be used on `settings_df` class objects."
    )
}

#' @export
arrange.snf_config <- function(.data, ...) {
    metasnf_error(
        "`arrange` cannot be used on `snf_config` class objects."
    )
}

#' @export
arrange.ari_matrix <- function(.data, ...) {
    metasnf_error(
        "`arrange` cannot be used on `weights_matrix` class objects."
    )
}
