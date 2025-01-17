#' Validator for `ext_solutions_df` class object
#'
#' @param ext_sol_dfl An extended solutions data frame-like object.
#' @return If ext_sol_dfl has a valid structure for an object of class
#'  ext_solutions_df, returns ext_sol_dfl. Otherwise, raises an error.
#' @export
validate_ext_solutions_df <- function(ext_sol_dfl) {
    return(ext_sol_dfl)
}

#' Constructor for `ext_solutions_df` class object
#'
#' @inheritParams validate_ext_solutions_df 
#' @return RETURN
#' @export
new_ext_solutions_df <- function(ext_sol_dfl) {
    ext_sol_df <- structure(
        ext_sol_dfl, class = c("ext_solutions_df", "data.frame")
    )    
    return(ext_sol_df)
}

