#' Function to extend dplyr to solutions data frame objects
#'
#' @param data A solutions data frame.
#' @param i A vector of row indices.
#' @param ... Additional arguments.
#'
#' @return Row sliced object with appropriately preserved attributes.
#' @export
dplyr_row_slice.solutions_df <- function(data, i, ...) {
    result <- NextMethod()
    attr(result, "sim_mats_list") <- attr(data, "sim_mats_list")[i]
    attr(result, "snf_config") <- attr(data, "snf_config")[i]
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

#' Function to extend dplyr to extended solutions data frame objects
#'
#' @param data An extended solutions data frame.
#' @param i A vector of row indices.
#' @param ... Additional arguments.
#'
#' @return Row sliced object with appropriately preserved attributes.
#' @export
dplyr_row_slice.ext_solutions_df <- function(data, i, ...) {
    result <- NextMethod()
    attr(result, "sim_mats_list") <- attr(data, "sim_mats_list")[i]
    attr(result, "snf_config") <- attr(data, "snf_config")[i]
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
