#' Extract mTBI subjects with a minimum time-since-last-mtbi threshold
#'
#' @param tbi_df A TBI dataframe
#' @param min_latest_mtbi_mpi The minimum time-since-last-mtbi to be selected
#'
#' @return subjects subjectkey values of selected subjects
#'
#' @export
#'
get_mtbi_subjects <- function(tbi_df, min_latest_mtbi_mpi) {
    subjects <- tbi_df |>
        dplyr::filter("mtbi" == 1 & "latest_mtbi_mpi" >= min_latest_mtbi_mpi) |>
        dplyr::select("subjectkey")
    return(subjects)
}
