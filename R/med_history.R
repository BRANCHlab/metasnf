#' Get number of mtbis sustained by subject
#'
#' @param otbi01 The baseline TBI dataframe
#' @param subjects Dataframe containing list of required subjects
#'
#' @return mtbi_count Dataframe containing number of previous mTBIs
#'
#' @export
get_mtbi_count <- function(otbi01, subjects = NULL) {
    mtbi_count <- detail_mtbi(otbi01, subjects) |>
        dplyr::select(
            "subjectkey",
            "mtbi_count"
        )
    return(mtbi_count)
}


#' Get subject headache history
#'
#' @param mx01 Dataframe containing medical history
#' @param subjects Dataframe containing list of required subjects
#'
#' @return headaches Dataframe containing headache history
#'
#' @export
get_headaches <- function(mx01, subjects = NULL) {
    headaches <- abcd_import(mx01, subjects) |>
        dplyr::rename("headache" = "medhx_2q") |>
        dplyr::select("subjectkey", "headache")
    return(headaches)
}
