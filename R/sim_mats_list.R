#' Validator for `similarity_matrix_list` class object
#'
#' @param PARAM1
#' @return RETURN
#' @export
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
#' @inheritParams validate_sim_mats_list
#' @return RETURN
#' @export
new_sim_mats_list <- function(smll) {
    sml <- structure(smll, class = c("sim_mats_list", "list"))
    return(sml)
}
