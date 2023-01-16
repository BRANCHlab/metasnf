#' Extract mTBI subjects with a minimum time-since-last-mtbi threshold
#'
#' @param otbi01 A TBI dataframe
#' @param min_latest_mtbi_mpi The minimum time-since-last-mtbi to be selected
#'
#' @return subjects Dataframe containing list of required subjects
#'
#' @export
#'
get_mtbi_subjects <- function(otbi01, min_latest_mtbi_mpi) {
    otbi01 <- otbi01 |>
        remove_dd() |>
        rename_tbi() |>
        identify_all_tbi() |>
        identify_mtbi() |>
        identify_mtbi_times()
    subjects <- otbi01 |>
        dplyr::filter(otbi01$"mtbi" == 1 &
                      otbi01$"latest_mtbi_mpi" >= min_latest_mtbi_mpi) |>
        dplyr::select("subjectkey")
    return(subjects)
}
