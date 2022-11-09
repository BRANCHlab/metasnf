#' Get acute symptom input variable 'latest_mtbi_mechanism'
#'
#' @param otbi01 The baseline TBI dataframe
#' @param subjects Dataframe containing list of required subjects
#' @param format Variable indicating if df is dummied or undummied
#'
#' @return mtbi_mechanism Dataframe containing latest_mtbi_mechanism
#'
#' @export
get_mtbi_mechanism <- function(otbi01, subjects = NULL, format = "dummied") {
    mtbi_mechanism <- detail_mtbi(otbi01, subjects) |>
        dplyr::select(
            "subjectkey",
            "latest_mtbi_mechanism"
        )
    if (format != "dummied" && format != "undummied") {
        print("Fomat must either be 'dummied' or 'undummied'.")
        return(NULL)
    } else if (format == "dummied") {
        mtbi_mechanism <- mtbi_mechanism |>
            dummy(cols = "latest_mtbi_mechanism")
    }
    return(stats::na.omit(mtbi_mechanism))
}

#' Get acute symptom input variable 'latest_mtbi_loc'
#'
#' @param otbi01 The baseline TBI dataframe
#' @param subjects Dataframe containing list of required subjects
#'
#' @return mtbi_loc Dataframe containing latest_mtbi_loc
#'
#' @export
get_mtbi_loc <- function(otbi01, subjects = NULL) {
    mtbi_loc <- detail_mtbi(otbi01, subjects) |>
        dplyr::select(
            "subjectkey",
            "latest_mtbi_loc"
        )
    return(stats::na.omit(mtbi_loc))
}

#' Get acute symptom input variable 'latest_mtbi_mem_daze'
#'
#' @param otbi01 The baseline TBI dataframe
#' @param subjects Dataframe containing list of required subjects
#'
#' @return mtbi_mem_daze Dataframe containing latest_mtbi_mem_daze
#'
#' @export
get_mtbi_mem_daze <- function(otbi01, subjects = NULL) {
    mtbi_mem_daze <- detail_mtbi(otbi01, subjects) |>
        dplyr::select(
            "subjectkey",
            "latest_mtbi_mem_daze"
        )
    return(stats::na.omit(mtbi_mem_daze))
}
