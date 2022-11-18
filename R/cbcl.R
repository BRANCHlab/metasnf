#' Get CBCL headache data
#'
#' @param abcd_cbcl01 NDA cbcl dataframe
#' @param subjects list of subjects to receive data for
#'
#' @return cbcl_headaches headache data
#'
#' @export
get_cbcl_headaches <- function(abcd_cbcl01, subjects) {
    cbcl_full <- abcd_import(abcd_cbcl01, subjects)
    cbcl_headaches <- cbcl_full |>
        dplyr::select(
            "subjectkey",
            "cbcl_q56b_p") |>
        dplyr::rename("cbcl_headaches" = "cbcl_q56b_p")
    return(cbcl_headaches)
}

#' Get CBCL nausea data
#'
#' @param abcd_cbcl01 NDA cbcl dataframe
#' @param subjects list of subjects to receive data for
#'
#' @return cbcl_nausea nausea data
#'
#' @export
get_cbcl_nausea <- function(abcd_cbcl01, subjects = NULL) {
    cbcl_full <- abcd_import(abcd_cbcl01, subjects)
    cbcl_nausea <- cbcl_full |>
        dplyr::select(
            "subjectkey",
            "cbcl_q56c_p") |>
        dplyr::rename("cbcl_nausea" = "cbcl_q56c_p")
    return(cbcl_nausea)
}

#' Get CBCL vomiting data
#'
#' @param abcd_cbcl01 NDA cbcl dataframe
#' @param subjects list of subjects to receive data for
#'
#' @return cbcl_vomiting vomiting data
#'
#' @export
get_cbcl_vomiting <- function(abcd_cbcl01, subjects = NULL) {
    cbcl_full <- abcd_import(abcd_cbcl01, subjects)
    cbcl_vomiting <- cbcl_full |>
        dplyr::select(
            "subjectkey",
            "cbcl_q56g_p") |>
        dplyr::rename("cbcl_vomiting" = "cbcl_q56g_p")
        return(cbcl_vomiting)
}

#' Get CBCL dizzy data
#'
#' @param abcd_cbcl01 NDA cbcl dataframe
#' @param subjects list of subjects to receive data for
#'
#' @return cbcl_dizzy dizzy data
#'
#' @export
get_cbcl_dizzy <- function(abcd_cbcl01, subjects = NULL) {
    cbcl_full <- abcd_import(abcd_cbcl01, subjects)
    cbcl_dizzy <- cbcl_full |>
        dplyr::select(
            "subjectkey",
            "cbcl_q51_p") |>
        dplyr::rename("cbcl_dizzy" = "cbcl_q51_p")
        return(cbcl_dizzy)
}

#' Get CBCL overtired data
#'
#' @param abcd_cbcl01 NDA cbcl dataframe
#' @param subjects list of subjects to receive data for
#'
#' @return cbcl_overtired overtired data
#'
#' @export
get_cbcl_overtired <- function(abcd_cbcl01, subjects = NULL) {
    cbcl_full <- abcd_import(abcd_cbcl01, subjects)
    cbcl_overtired <- cbcl_full |>
        dplyr::select(
            "subjectkey",
            "cbcl_q54_p") |>
        dplyr::rename("cbcl_overtired" = "cbcl_q54_p")
        return(cbcl_overtired)
}

#' Get CBCL sleeping_more data
#'
#' @param abcd_cbcl01 NDA cbcl dataframe
#' @param subjects list of subjects to receive data for
#'
#' @return cbcl_sleeping_more sleeping_more data
#'
#' @export
get_cbcl_sleeping_more <- function(abcd_cbcl01, subjects = NULL) {
    cbcl_full <- abcd_import(abcd_cbcl01, subjects)
    cbcl_sleeping_more <- cbcl_full |>
        dplyr::select(
            "subjectkey",
            "cbcl_q77_p") |>
        dplyr::rename("cbcl_sleeping_more" = "cbcl_q77_p")
        return(cbcl_sleeping_more)
}

#' Get CBCL sleeping_less data
#'
#' @param abcd_cbcl01 NDA cbcl dataframe
#' @param subjects list of subjects to receive data for
#'
#' @return cbcl_sleeping_less sleeping_less data
#'
#' @export
get_cbcl_sleeping_less <- function(abcd_cbcl01, subjects = NULL) {
    cbcl_full <- abcd_import(abcd_cbcl01, subjects)
    cbcl_sleeping_less <- cbcl_full |>
        dplyr::select(
            "subjectkey",
            "cbcl_q76_p") |>
        dplyr::rename("cbcl_sleeping_less" = "cbcl_q76_p")
        return(cbcl_sleeping_less)
}

#' Get CBCL depression data
#'
#' @param abcd_cbcl01 NDA cbcl dataframe
#' @param subjects list of subjects to receive data for
#'
#' @return cbcl_depress_r depression data
#'
#' @export
get_cbcl_depress_r <- function(abcd_cbcls01, subjects = NULL) {
    cbcl_full <- abcd_import(abcd_cbcls01, subjects)
    cbcl_depress_r <- cbcl_full |>
        dplyr::select(
            "subjectkey",
            "cbcl_scr_dsm5_depress_r") |>
        dplyr::rename("cbcl_depress_r" = "cbcl_scr_dsm5_depress_r")
        return(cbcl_depress_r)
}

#' Get CBCL anxiety data
#'
#' @param abcd_cbcl01 NDA cbcl dataframe
#' @param subjects list of subjects to receive data for
#'
#' @return cbcl_anxiety_r anxiety data
#'
#' @export
get_cbcl_anxiety_r <- function(abcd_cbcls01, subjects = NULL) {
    cbcl_full <- abcd_import(abcd_cbcls01, subjects)
    cbcl_anxiety_r <- cbcl_full |>
        dplyr::select(
            "subjectkey",
            "cbcl_scr_dsm5_anxdisord_r") |>
        dplyr::rename("cbcl_anxiety_r" = "cbcl_scr_dsm5_anxdisord_r")
        return(cbcl_anxiety_r)
}

#' Get CBCL attention data
#'
#' @param abcd_cbcl01 NDA cbcl dataframe
#' @param subjects list of subjects to receive data for
#'
#' @return cbcl_attention_r attention data
#'
#' @export
get_cbcl_attention_r <- function(abcd_cbcls01, subjects = NULL) {
    cbcl_full <- abcd_import(abcd_cbcls01, subjects)
    cbcl_attention_r <- cbcl_full |>
        dplyr::select(
            "subjectkey",
            "cbcl_scr_syn_attention_r") |>
        dplyr::rename("cbcl_attention_r" = "cbcl_scr_syn_attention_r")
        return(cbcl_attention_r)
}


#' Get CBCL aggressive data
#'
#' @param abcd_cbcl01 NDA cbcl dataframe
#' @param subjects list of subjects to receive data for
#'
#' @return cbcl_aggressive_r aggressive data
#'
#' @export
get_cbcl_aggressive_r <- function(abcd_cbcls01, subjects = NULL) {
    cbcl_full <- abcd_import(abcd_cbcls01, subjects)
    cbcl_aggressive_r <- cbcl_full |>
        dplyr::select(
            "subjectkey",
            "cbcl_scr_syn_aggressive_r") |>
        dplyr::rename("cbcl_aggressive_r" = "cbcl_scr_syn_aggressive_r")
        return(cbcl_aggressive_r)
}
