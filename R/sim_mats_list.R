#' Validator for `similarity_matrix_list` class object
#'
#' @keywords internal
#' @param smll A similarity matrix list-like object.
#' @return If smll has a valid structure for class `similarity_matrix_list`,
#'  returns smll. Otherwise, raises an error.
validate_sim_mats_list <- function(smll) {
    all_null_or_mat <- lapply(
        smll,
        function(x) {
            is.matrix(x) | is.null(x)
        }
    ) |>
        unlist() |>
        all()
    if (!all_null_or_mat) {
        metasnf_error(
            "Similarity matrix list should be a list of NULL or matrices."
        )
    }
    return(smll)
}

#' Constructor for `similarity_matrix_list` class object
#'
#' @keywords internal
#' @inheritParams validate_sim_mats_list
#' @return A `similarity_matrix_list` class object.
new_sim_mats_list <- function(smll) {
    sml <- structure(smll, class = c("sim_mats_list", "list"))
    return(sml)
}

#' Create or extract a `sim_mats_list` class object
#'
#' @param x The object to create or extract a `sim_mats_list` from.
#' @return A `sim_mats_list` class object.
#' @export
sim_mats_list <- function(x) {
    UseMethod("sim_mats_list")
}

#' @export
sim_mats_list.default <- function(x) {
    return(attributes(x)$"sim_mats_list")
}

#' @export
sim_mats_list.solutions_df <- function(x) {
    return(attributes(x)$"sim_mats_list")
}

#' @export
sim_mats_list.ext_solutions_df <- function(x) {
    return(attributes(x)$"sim_mats_list")
}
