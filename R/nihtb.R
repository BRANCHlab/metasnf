#' Get nihtbx list sorting data
#'
#' @param abcd_tbss01 NDA nihtbx dataframe
#' @param subjects list of subjects to receive data for
#'
#' @return nihtbx_list_fc list sorting data
#'
#' @export
get_nihtbx_list_fc <- function(abcd_tbss01, subjects = NULL) {
    nihtbx_full <- abcd_import(abcd_tbss01, subjects)
    nihtbx_list_fc <- nihtbx_full |>
        dplyr::select(
            "subjectkey",
            "nihtbx_list_fc")
        return(nihtbx_list_fc)
}

#' Get nihtbx cardsort data
#'
#' @param abcd_tbss01 NDA nihtbx dataframe
#' @param subjects cardsort of subjects to receive data for
#'
#' @return nihtbx_cardsort_fc cardsort data
#'
#' @export
get_nihtbx_cardsort_fc <- function(abcd_tbss01, subjects = NULL) {
    nihtbx_full <- abcd_import(abcd_tbss01, subjects)
    nihtbx_cardsort_fc <- nihtbx_full |>
        dplyr::select(
            "subjectkey",
            "nihtbx_cardsort_fc")
        return(nihtbx_cardsort_fc)
}

#' Get nihtbx pattern data
#'
#' @param abcd_tbss01 NDA nihtbx dataframe
#' @param subjects pattern of subjects to receive data for
#'
#' @return nihtbx_pattern_fc pattern data
#'
#' @export
get_nihtbx_pattern_fc <- function(abcd_tbss01, subjects = NULL) {
    nihtbx_full <- abcd_import(abcd_tbss01, subjects)
    nihtbx_pattern_fc <- nihtbx_full |>
        dplyr::select(
            "subjectkey",
            "nihtbx_pattern_fc")
        return(nihtbx_pattern_fc)
}
