#' Helper function for raising errors.
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

#' Helper function for raising warnings.
#'
#' @keywords internal
#' @param ... Arbitrary number of strings to be pasted together into warning
#'  message.
#' @param env 
#' @return Returns no value. Raises an error through cli::cli_abort.
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
#' @param env 
#' @return Returns no value. Raises an error through cli::cli_abort.
metasnf_alert <- function(..., env = 1) {
    cli::cli_alert_info(
        text = paste0( ...),
        .envir = rlang::caller_env(env)
    )
}
