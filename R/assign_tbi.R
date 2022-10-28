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
    dft <- tbi_df |>
        dplyr::mutate(
            dplyr::across(
                "hosp_er_inj":"other_other_multi_effect_end_age", as.numeric))
    dft$"interview_age" <- as.numeric(dft$"interview_age")
    # Generate mtbi and moderate_or_severe_tbi columns
    dfm <- dft |> dplyr::mutate(
        mtbi = dplyr::case_when(
            (dft$"hosp_er_loc" < 2 & dft$"hosp_er_mem_daze" == 1) |
                dft$"hosp_er_loc" == 1 ~ 1,
            (dft$"vehicle_loc" < 2 & dft$"vehicle_mem_daze" == 1) |
                dft$"vehicle_loc" == 1 ~ 1,
            (dft$"fall_hit_loc" < 2 & dft$"fall_hit_mem_daze" == 1) |
                dft$"fall_hit_loc" == 1 ~ 1,
            (dft$"violent_loc" < 2 & dft$"violent_mem_daze" == 1) |
                dft$"violent_loc" == 1 ~ 1,
            (dft$"blast_loc" < 2 & dft$"blast_mem_daze" == 1) |
                dft$"blast_loc" == 1 ~ 1,
            (dft$"other_loc_num" - dft$"other_loc_num_over_30") > 0 ~ 1,
            (dft$"multi_loc" < 2 & dft$"multi_mem_daze" == 1) |
                dft$"multi_loc" == 1 ~ 1,
            dft$"other_multi_inj" == 1 ~ 0.5,
            dft$"other_other_multi_inj" == 1 ~ 0.5,
            TRUE ~ 0),
        moderate_or_severe_tbi = dplyr::case_when(
            (dft$"hosp_er_loc" > 1 |
                dft$"vehicle_loc" > 1 |
                dft$"fall_hit_loc" > 1 |
                dft$"violent_loc" > 1 |
                dft$"blast_loc" > 1 |
                dft$"other_loc_num_over_30" > 0 |
                dft$"multi_loc" > 1) ~ 1,
            dft$"other_multi_inj" == 1 ~ 0.5,
            dft$"other_other_multi_inj" == 1 ~ 0.5,
            TRUE ~ 0))
    # Generate columns indicating which injury types were mTBIs
    dfl <- dfm |> dplyr::mutate(
        hosp_er_mtbi = dplyr::case_when((
                dfm$"hosp_er_loc" < 2 &
                dfm$"hosp_er_mem_daze" == 1) |
                dfm$"hosp_er_loc" == 1 ~ 1,
                TRUE ~ 0),
        vehicle_mtbi = dplyr::case_when((
                dfm$"vehicle_loc" < 2 & dfm$"vehicle_mem_daze" == 1) |
                dfm$"vehicle_loc" == 1 ~ 1,
                TRUE ~ 0),
        fall_hit_mtbi = dplyr::case_when((
                dfm$"fall_hit_loc" < 2 & dfm$"fall_hit_mem_daze" == 1) |
                dfm$"fall_hit_loc" == 1 ~ 1,
                TRUE ~ 0),
        violent_mtbi = dplyr::case_when((
                dfm$"violent_loc" < 2 & dfm$"violent_mem_daze" == 1) |
                dfm$"violent_loc" == 1 ~ 1,
                TRUE ~ 0),
        blast_mtbi = dplyr::case_when((
                dfm$"blast_loc" < 2 & dfm$"blast_mem_daze" == 1) |
                blast_loc == 1 ~ 1,
                TRUE ~ 0),
        other_loc_mtbi = dplyr::case_when((
                dfm$"other_loc_num" - dfm$"other_loc_num_over_30") > 0 ~
                    dfm$"other_loc_num" - dfm$"other_loc_num_over_30",
                TRUE ~ 0),
        multi_mtbi = dplyr::case_when((
                dfm$"multi_loc" < 2 & dfm$"multi_mem_daze" == 1) |
                dfm$"multi_loc" == 1 ~ 1,
                TRUE ~ 0))
    # Scale injury ages to match interview ages if necessary
    if (mean(dfl$"hosp_er_age", na.rm = TRUE) < 20) {
        print(paste0("Note: Interview ages are provided in months, but ",
            "injury ages are provided in years. Injury ages will be scaled ",
            "up to determine time between interview and latest mTBI."))
        dfl$"blast_age" <-
            dfl$"blast_age" * 12
        dfl$"hosp_er_age" <-
            dfl$"hosp_er_age" * 12
        dfl$"vehicle_age" <-
            dfl$"vehicle_age" * 12
        dfl$"fall_hit_age" <-
            dfl$"fall_hit_age" * 12
        dfl$"violent_age" <-
            dfl$"violent_age" * 12
        dfl$"other_loc_min_age" <-
            dfl$"other_loc_min_age" * 12
        dfl$"multi_effect_end_age" <-
            dfl$"multi_effect_end_age" * 12
    }
    # Generate columns indicating time since mTBI of specific type
    dft1 <- dfl |> dplyr::mutate(
        hosp_er_mtbi_mpi = dplyr::case_when(
            dfl$"hosp_er_mtbi" == 1 ~
                dfl$"interview_age" - dfl$"hosp_er_age"),
        vehicle_mtbi_mpi = dplyr::case_when(
            dfl$"vehicle_mtbi" == 1 ~
                dfl$"interview_age" - dfl$"vehicle_age"),
        fall_hit_mtbi_mpi = dplyr::case_when(
            dfl$"fall_hit_mtbi" == 1 ~
                dfl$"interview_age" - dfl$"fall_hit_age"),
        violent_mtbi_mpi = dplyr::case_when(
            dfl$"violent_mtbi" == 1 ~
                dfl$"interview_age" - dfl$"violent_age"),
        blast_mtbi_mpi = dplyr::case_when(
            dfl$"blast_mtbi" == 1 ~
                dfl$"interview_age" - dfl$"blast_age"),
        other_loc_mtbi_mpi = dplyr::case_when(
            dfl$"other_loc_mtbi" == 1 ~
                dfl$"interview_age" - dfl$"other_loc_min_age"),
        multi_mtbi_mpi = dplyr::case_when(
            dfl$"multi_mtbi" == 1 ~
                dfl$"interview_age" - dfl$"multi_effect_end_age"))
    # Generate column indicating time since latest mTBI
    dft2 <- dft1 |>
        dplyr::mutate(latest_mtbi_mpi = pmin(
            dft1$"hosp_er_mtbi_mpi",
            dft1$"vehicle_mtbi_mpi",
            dft1$"fall_hit_mtbi_mpi",
            dft1$"violent_mtbi_mpi",
            dft1$"blast_mtbi_mpi",
            dft1$"other_loc_mtbi_mpi",
            dft1$"multi_mtbi_mpi",
            na.rm = TRUE
        ))
    # Generate column for best guess of number of mTBIs sustained
    dfc <- dft2 |>
        dplyr::mutate(mtbi_count = (
            dft2$"hosp_er_mtbi" +
            dft2$"vehicle_mtbi" +
            dft2$"fall_hit_mtbi" +
            dft2$"violent_mtbi" +
            dft2$"blast_mtbi" +
            dft2$"other_loc_mtbi" +
            dft2$"multi_mtbi"))
    return(dfc)
}
