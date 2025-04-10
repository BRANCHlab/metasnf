#' Helper function for raising errors
#'
#' @keywords internal
#' @param ... Arbitrary number of strings to be pasted together into error
#'  message.
#' @param env 
#' @return Returns no value. Raises an error through cli::cli_abort.
metasnf_error <- function(..., env = 1) {
    cli::cli_abort(
        message = c(
            x = paste0(
                ...
            )
        ),
        .envir = rlang::caller_env(env)
    )
}

#' Helper function for raising warnings
#'
#' @keywords internal
#' @param ... Arbitrary number of strings to be pasted together into warning
#'  message.
#' @param env Environment to evaluate expressions in.
#' @return Returns no value. Raises a warning through cli::cli_warn
metasnf_warning <- function(..., env = 1) {
    cli::cli_warn(
        message = c(
            "!" = paste0(
                ...
            )
        ),
        .envir = rlang::caller_env(env)
    )
}

#' Helper function for raising alerts
#'
#' @keywords internal
#' @param ... Arbitrary number of strings to be pasted together into alert
#'  message.
#' @param env Environment to evaluate expressions in.
#' @return Returns no value. Raises an alert through cli::cli_alert_info.
metasnf_alert <- function(..., env = 1) {
    cli::cli_alert_info(
        text = paste0( ...),
        .envir = rlang::caller_env(env)
    )
}

#' Helper function for deprecated function warnings
#'
#' @keywords internal
#' @param version Version of `metasnf` in which function has been deprecated.
#' @param alternative Recommended alternative approach.
#' @param env Environment to evaluate expressions in.
#' @return Returns no value. Raises a warning through cli::cli_warn.
metasnf_deprecated <- function(version, alternative, env = 1) {
    cli::cli_warn(
        message = c(
            "!" = paste0(
                "`", sys.call(-1)[[1]], "()` has been deprecated as of",
                " `metasnf` version ", version, ".", alternative
            )
        ),
        .envir = rlang::caller_env(env)
    )
}

#' Helper function for defunct function errors
#'
#' @keywords internal
#' @param version Version of `metasnf` in which function has been made defunct.
#' @param alternative Recommended alternative approach.
#' @param env Environment to evaluate expressions in.
#' @return Returns no value. Raises an error through cli::cli_abort.
metasnf_defunct <- function(version, alternative, env = 1) {
    cli::cli_abort(
        message = c(
            "!" = paste0(
                "`", sys.call(-1)[[1]], "()` has been made defunct as of",
                " `metasnf` version ", version, ".", alternative
            )
        ),
        .envir = rlang::caller_env(env)
    )
}
