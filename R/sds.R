#' Sleep disturbance scale
#'
#' @param abcd_sds01 Dataframe containing sleep disturbance scale data
#' @param subjects list of subjects to receive data for
#'
#' @return full_sleep_df Dataframe containing sleep data
#'
#' @export
get_full_sleep_df <- function(abcd_sds01, subjects = NULL) {
    sds_full <- abcd_import(abcd_sds01, subjects)
    full_sleep_df <- sds_full |>
        dplyr::select(
            "subjectkey",
            "sleepdisturb1_p":"sleepdisturb26_p") |>
        dplyr::rename(
            "sleep_hours" = "sleepdisturb1_p",
            "sleep_time_to" = "sleepdisturb2_p",
            "sleep_reluctance" = "sleepdisturb3_p",
            "sleep_difficulty" = "sleepdisturb4_p",
            "sleep_anxiety" = "sleepdisturb5_p",
            "sleep_startles" = "sleepdisturb6_p",
            "sleep_repetitive" = "sleepdisturb7_p",
            "sleep_vivid_dream" = "sleepdisturb8_p",
            "sleep_sweats_falling" = "sleepdisturb9_p",
            "sleep_wakes_up_multiple" = "sleepdisturb10_p",
            "sleep_hard_to_sleep_again" = "sleepdisturb11_p",
            "sleep_twitching" = "sleepdisturb12_p",
            "sleep_breathing" = "sleepdisturb13_p",
            "sleep_gasps" = "sleepdisturb14_p",
            "sleep_snores" = "sleepdisturb15_p",
            "sleep_sweats_sleeping" = "sleepdisturb16_p",
            "sleep_walks" = "sleepdisturb17_p",
            "sleep_talks" = "sleepdisturb18_p",
            "sleep_grinds_teeth" = "sleepdisturb19_p",
            "sleep_wakes_screaming" = "sleepdisturb20_p",
            "sleep_forgets_nightmares" = "sleepdisturb21_p",
            "sleep_difficult_to_wake" = "sleepdisturb22_p",
            "sleep_wakes_up_tired" = "sleepdisturb23_p",
            "sleep_cant_move_morning" = "sleepdisturb24_p",
            "sleep_daytime_sleepy" = "sleepdisturb25_p",
            "sleep_suddenly_sleeps" = "sleepdisturb26_p")
    full_sleep_df <- full_sleep_df |>
        dplyr::mutate_at(c(2:length(full_sleep_df)), as.numeric)
    full_sleep_df$"sleep_total_problems" <-
        rowSums(full_sleep_df[, 2:length(full_sleep_df)])
    return(full_sleep_df)
}
