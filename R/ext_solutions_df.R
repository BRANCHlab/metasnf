#' Constructor for `ext_solutions_df` class object.
#'
#' The extended solutions data frame is a column-extended variation of the
#' solutions data frame. It contains association p-values relating cluster
#' membership to feature distribution for all solutions in a solutions data
#' frame and all features in a provided data list (or data lists). If a
#' target data list was used during the call to `extend_solutions`, the 
#' extended solutions data frame will also have columns "min_pval",
#' "mean_pval", and "max_pval" summarizing the p-values of just those features
#' that were a part of the target list.
#'
#' @keywords internal
#' @inheritParams extend_solutions
#' @param ext_sol_dfl An extended solutions data frame-like object.
#' @param fts A vector of all features that have association p-values stored
#'  in the resulting extended solutions data frame.
#' @return An `ext_solutions_df` class object.
ext_solutions_df <- function(ext_sol_dfl, sol_df, fts, target_dl) {
    ext_sol_dfl <- numcol_to_numeric(ext_sol_dfl)
    ext_sol_dfl$"solution" <- as.factor(ext_sol_dfl$"solution")
    ext_sol_dfl <- dplyr::inner_join(sol_df, ext_sol_dfl, by = "solution")
    attributes(ext_sol_dfl)$"features" <- fts
    attributes(ext_sol_dfl)$"snf_config" <- attributes(sol_df)$"snf_config"
    class(ext_sol_dfl) <- c("ext_solutions_df", "data.frame")
    if (!is.null(target_dl)) {
        ext_sol_dfl <- dplyr::select(
            ext_sol_dfl,
            "solution",
            "nclust",
            "mc",
            "min_pval",
            "mean_pval",
            "max_pval",
            dplyr::everything()
        )
        attributes(ext_sol_dfl)$"summary_features" <- features(target_dl)
    } else {
        attributes(ext_sol_dfl)$"summary_features" <- as.character()
    }
    ext_sol_dfl <- validate_ext_solutions_df(ext_sol_dfl)
    ext_sol_df <- new_ext_solutions_df(ext_sol_dfl)
    return(ext_sol_df)
}

#' Validator for `ext_solutions_df` class object
#'
#' @keywords internal
#' @param ext_sol_dfl An extended solutions data frame-like object.
#' @return If ext_sol_dfl has a valid structure for an object of class
#'  ext_solutions_df, returns ext_sol_dfl. Otherwise, raises an error.
validate_ext_solutions_df <- function(ext_sol_dfl) {
    pval_cols <- dplyr::select(ext_sol_dfl, dplyr::ends_with("_pval"))
    if (!(length(pval_cols) > 1)) {
        metasnf_error(
            "Extended solutions data frame must have at least one p-value",
            " column."
        )
    }
    if (is.null(sim_mats_list(ext_sol_dfl))) {
        metasnf_error(
            "Extended solutions data frame must have `sim_mats_list`",
            " attributes."
        )
    }
    return(ext_sol_dfl)
}

#' Constructor for `ext_solutions_df` class object
#'
#' @keywords internal
#' @inheritParams validate_ext_solutions_df 
#' @return An `ext_solutions_df` object, which is a data frame with class
#'  `ext_solutions_df`.
new_ext_solutions_df <- function(ext_sol_dfl) {
    ext_sol_df <- structure(
        ext_sol_dfl, class = c("ext_solutions_df", "data.frame")
    )    
    return(ext_sol_df)
}
