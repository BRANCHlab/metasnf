#' Select columns relevant for acute symptoms
#'
#' @param tbi_df A TBI dataframe
#'
#' @return acute_symptoms Dataframe containing acute symptom information
#'
#' @export
get_acute_symptoms <- function(tbi_df) {
    acute_symptoms <- tbi_df |>
        dplyr::select(
            "subjectkey",
            "latest_mtbi_mechanism",
            "latest_mtbi_age",
            "latest_mtbi_loc",
            "latest_mtbi_mem_daze"
        )
    return(acute_symptoms)
}
