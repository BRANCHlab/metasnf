#' Returns race information
#'
#' @param med_df Dataframe containing medical history information
#' @param detailed_tbi_df Dataframe containing tbi information
#'
#' @return med_history_df Dataframe containing important medical history vars
#'
#' @export
get_med_history <- function(med_df, detailed_tbi_df) {
    headaches <- med_df |>
        dplyr::rename("headache" = "medhx_2q") |>
        dplyr::select("subjectkey", "headache")
    mtbi_count <- detailed_tbi_df |>
        dplyr::select("subjectkey", "mtbi_count")
    med_history_df <-
        dplyr::inner_join(headaches, mtbi_count, by = "subjectkey")
    return(med_history_df)
}
