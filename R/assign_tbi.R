#' Adds columns to tbi_df showing if a child has had an mTBI or moderate+ TBI
#'
#' Calculates whether or not child had an mTBI using the following definition:
#' (0 mins < LOC < 30 mins) OR (LOC < 30 mins AND felt dazed or confused)
#'
#' Calculates moderate or severe TBIs using the following definition:
#' (LOC > 30 mins)
#'
#' These definitions aim to match typical GCS-based definitions as well as
#' possible.
#'
#' @param tbi_df A TBI dataframe
#'
#' @return renamed_tbi A modified form of tbi_df with clearer column names
#'
#' @export
#'
assign_tbi <- function(tbi_df) {
    print("Note: This function converts the mTBI column types to numeric.")

    # Assign column types
    df <- tbi_df |>
        dplyr::mutate(
            dplyr::across(
                "hosp_er_inj":"other_other_multi_effect_end_age", as.numeric))

    # Generate mtbi and moderate_or_severe_tbi columns
    tbi_df_labeled <- df |> dplyr::mutate(
        mtbi = dplyr::case_when(
            (df$"hosp_er_loc" < 2 & df$"hosp_er_mem_daze" == 1) |
                df$"hosp_er_loc" == 1 ~ 1,
            (df$"vehicle_loc" < 2 & df$"vehicle_mem_daze" == 1) |
                df$"vehicle_loc" == 1 ~ 1,
            (df$"fall_hit_loc" < 2 & df$"fall_hit_mem_daze" == 1) |
                df$"fall_hit_loc" == 1 ~ 1,
            (df$"violent_hit_loc" < 2 & df$"violent_hit_mem_daze" == 1) |
                df$"violent_hit_loc" == 1 ~ 1,
            (df$"blast_loc" < 2 & df$"blast_mem_daze" == 1) |
                df$"blast_loc" == 1 ~ 1,
            (df$"other_loc_num" - df$"other_loc_num_over_30") > 0 ~ 1,
            (df$"multi_loc" < 2 & df$"multi_mem_daze" == 1) |
                df$"multi_loc" == 1 ~ 1,
            df$"other_multi_inj" == 1 ~ 0.5,
            df$"other_other_multi_inj" == 1 ~ 0.5,
            TRUE ~ 0),
        moderate_or_severe_tbi = dplyr::case_when(
            (df$"hosp_er_loc" > 1 |
                df$"vehicle_loc" > 1 |
                df$"fall_hit_loc" > 1 |
                df$"violent_hit_loc" > 1 |
                df$"blast_loc" > 1 |
                df$"other_loc_num_over_30" > 0 |
                df$"multi_loc" > 1) ~ 1,
            df$"other_multi_inj" == 1 ~ 0.5,
            df$"other_other_multi_inj" == 1 ~ 0.5,
            TRUE ~ 0))

    # Scale injury ages to match interview ages if necessary
    if (mean(tbi_df_labeled$"hosp_er_age", na.rm = TRUE) < 20) {
        print(paste0("Note: Interview ages are provided in months, but injury",
            "ages are provided in years. Injury ages will be scaled up to",
            "determine time between interview and latest mTBI."))
        tbi_labeled$"blast_age" <-
            tbi_labeled$"blast_age" * 12
        tbi_labeled$"hosp_er_age" <-
            tbi_labeled$"hosp_er_age" * 12
        tbi_labeled$"vehicle_age" <-
            tbi_labeled$"vehicle_age" * 12
        tbi_labeled$"fall_hit_age" <-
            tbi_labeled$"fall_hit_age" * 12
        tbi_labeled$"violent_hit_age" <-
            tbi_labeled$"violent_hit_age" * 12
        tbi_labeled$"other_loc_min_age" <-
            tbi_labeled$"other_loc_min_age" * 12
        tbi_labeled$"multi_effect_end_age" <-
            tbi_labeled$"multi_effect_end_age" * 12
    }
    return(tbi_df_labeled)
}
