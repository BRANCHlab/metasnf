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
    dfct <- tbi_df |>
        dplyr::mutate(
            dplyr::across(
                "hosp_er_inj":"other_other_multi_effect_end_age", as.numeric))
    dfct$"interview_age" <- as.numeric(dfct$"interview_age")

    # Generate mtbi and moderate_or_severe_tbi columns
    dfm <- dfct |> dplyr::mutate(
        mtbi = dplyr::case_when(
            (dfct$"hosp_er_loc" < 2 & dfct$"hosp_er_mem_daze" == 1) |
                dfct$"hosp_er_loc" == 1 ~ 1,
            (dfct$"vehicle_loc" < 2 & dfct$"vehicle_mem_daze" == 1) |
                dfct$"vehicle_loc" == 1 ~ 1,
            (dfct$"fall_hit_loc" < 2 & dfct$"fall_hit_mem_daze" == 1) |
                dfct$"fall_hit_loc" == 1 ~ 1,
            (dfct$"violent_loc" < 2 & dfct$"violent_mem_daze" == 1) |
                dfct$"violent_loc" == 1 ~ 1,
            (dfct$"blast_loc" < 2 & dfct$"blast_mem_daze" == 1) |
                dfct$"blast_loc" == 1 ~ 1,
            (dfct$"other_loc_num" - dfct$"other_loc_num_over_30") > 0 ~ 1,
            (dfct$"multi_loc" < 2 & dfct$"multi_mem_daze" == 1) |
                dfct$"multi_loc" == 1 ~ 1,
            dfct$"other_multi_inj" == 1 ~ 0.5,
            dfct$"other_other_multi_inj" == 1 ~ 0.5,
            TRUE ~ 0),
        moderate_or_severe_tbi = dplyr::case_when(
            (dfct$"hosp_er_loc" > 1 |
                dfct$"vehicle_loc" > 1 |
                dfct$"fall_hit_loc" > 1 |
                dfct$"violent_loc" > 1 |
                dfct$"blast_loc" > 1 |
                dfct$"other_loc_num_over_30" > 0 |
                dfct$"multi_loc" > 1) ~ 1,
            dfct$"other_multi_inj" == 1 ~ 0.5,
            dfct$"other_other_multi_inj" == 1 ~ 0.5,
            TRUE ~ 0))

    # Generate columns indicating which injury types were mTBIs
    dfm2 <- dfm |> dplyr::mutate(
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
    if (mean(dfm2$"hosp_er_age", na.rm = TRUE) < 20) {
        print(paste0("Note: Interview ages are provided in months, but ",
            "injury ages are provided in years. Injury ages will be scaled ",
            "up to determine time between interview and latest mTBI."))
        dfm2$"blast_age" <-
            dfm2$"blast_age" * 12
        dfm2$"hosp_er_age" <-
            dfm2$"hosp_er_age" * 12
        dfm2$"vehicle_age" <-
            dfm2$"vehicle_age" * 12
        dfm2$"fall_hit_age" <-
            dfm2$"fall_hit_age" * 12
        dfm2$"violent_age" <-
            dfm2$"violent_age" * 12
        dfm2$"other_loc_min_age" <-
            dfm2$"other_loc_min_age" * 12
        dfm2$"multi_effect_end_age" <-
            dfm2$"multi_effect_end_age" * 12
    }

    # Generate columns indicating time since mTBI of specific type
    dft <- dfm2 |> dplyr::mutate(
        hosp_er_mtbi_mpi = dplyr::case_when(
            dfm2$"hosp_er_mtbi" == 1 ~
                dfm2$"interview_age" - dfm2$"hosp_er_age"),
        vehicle_mtbi_mpi = dplyr::case_when(
            dfm2$"vehicle_mtbi" == 1 ~
                dfm2$"interview_age" - dfm2$"vehicle_age"),
        fall_hit_mtbi_mpi = dplyr::case_when(
            dfm2$"fall_hit_mtbi" == 1 ~
                dfm2$"interview_age" - dfm2$"fall_hit_age"),
        violent_mtbi_mpi = dplyr::case_when(
            dfm2$"violent_mtbi" == 1 ~
                dfm2$"interview_age" - dfm2$"violent_age"),
        blast_mtbi_mpi = dplyr::case_when(
            dfm2$"blast_mtbi" == 1 ~
                dfm2$"interview_age" - dfm2$"blast_age"),
        other_loc_mtbi_mpi = dplyr::case_when(
            dfm2$"other_loc_mtbi" == 1 ~
                dfm2$"interview_age" - dfm2$"other_loc_min_age"),
        multi_mtbi_mpi = dplyr::case_when(
            dfm2$"multi_mtbi" == 1 ~
                dfm2$"interview_age" - dfm2$"multi_effect_end_age"))

    # Generate column indicating time since latest mTBI
    dft2 <- dft |>
        dplyr::mutate(latest_mtbi_mpi = pmin(
            dft$"hosp_er_mtbi_mpi",
            dft$"vehicle_mtbi_mpi",
            dft$"fall_hit_mtbi_mpi",
            dft$"violent_mtbi_mpi",
            dft$"blast_mtbi_mpi",
            dft$"other_loc_mtbi_mpi",
            dft$"multi_mtbi_mpi",
            na.rm = TRUE
        ))

    # Generate columns indicating age at latest mTBI
    dfa <- dft2 |>
        dplyr::mutate(latest_mtbi_age = pmax(
            dft2$"hosp_er_age",
            dft2$"vehicle_age",
            dft2$"fall_hit_age",
            dft2$"violent_age",
            dft2$"blast_age",
            dft2$"other_loc_min_age",
            dft2$"multi_effect_end_age",
            na.rm = TRUE
        ))


    # Generate column indicating mechanism of latest mTBI
    dfmech <- dfa |> dplyr::mutate(latest_mtbi_mechanism = dplyr::case_when(
        dfa$"latest_mtbi_age" == dfa$"hosp_er_age" ~
            "hosp_er_age",
        dfa$"latest_mtbi_age" == dfa$"vehicle_age" ~
            "vehicle_age",
        dfa$"latest_mtbi_age" == dfa$"fall_hit_age" ~
            "fall_hit_age",
        dfa$"latest_mtbi_age" == dfa$"violent_age" ~
            "violent_age",
        dfa$"latest_mtbi_age" == dfa$"blast_age" ~
            "blast_age",
        dfa$"latest_mtbi_age" == dfa$"other_loc_min_age" ~
            "other_loc_min_age",
        dfa$"latest_mtbi_age" == dfa$"multi_effect_end_age" ~
            "multi_effect_end_age"
        ))

    # Generate column for best guess of number of mTBIs sustained
    dfnt <- dfmech |>
        dplyr::mutate(mtbi_count = (
            dfmech$"hosp_er_mtbi" +
            dfmech$"vehicle_mtbi" +
            dfmech$"fall_hit_mtbi" +
            dfmech$"violent_mtbi" +
            dfmech$"blast_mtbi" +
            dfmech$"other_loc_mtbi" +
            dfmech$"multi_mtbi"))


    # Create LOC duration variable for the 'other_loc' category
    dfl <- dfnt |>
        dplyr::mutate(other_loc_loc = dplyr::case_when(
            dfnt$"other_loc_max_loc_mins" == 0 ~ 0,
            dfnt$"other_loc_max_loc_mins" > 0 &
                dfnt$"other_loc_max_loc_mins" <= 30 ~ 1,
            dfnt$"other_loc_max_loc_mins" > 30 &
                dfnt$"other_loc_max_loc_mins" <= 1440 ~ 2,
            dfnt$"other_loc_max_loc_mins" > 1440 ~ 3,
            TRUE ~ NA_real_
        ))

    # Create column indicating loc of latest mTBI
    dfll <- dfl |>
        dplyr::mutate(latest_mtbi_loc = dplyr::case_when(
            dfl$"latest_mtbi_mechanism" == "hosp_er" ~ hosp_er_loc,
            dfl$"latest_mtbi_mechanism" == "vehicle" ~ vehicle_loc,
            dfl$"latest_mtbi_mechanism" == "fall_hit" ~ fall_hit_loc,
            dfl$"latest_mtbi_mechanism" == "violent" ~ violent_loc,
            dfl$"latest_mtbi_mechanism" == "blast" ~ blast_loc,
            dfl$"latest_mtbi_mechanism" == "other_loc" ~ other_loc_loc,
            dfl$"latest_mtbi_mechanism" == "multi" ~ multi_loc,
            TRUE ~ NA_real_
        ))

    # Generate column indicating if latest mTBI had memory loss / dazed feeling
    dfml <- dfll |>
        dplyr::mutate(latest_mtbi_mem_daze = dplyr::case_when(
            dfll$"latest_mtbi_mechanism" == "hosp_er" ~ hosp_er_mem_daze,
            dfll$"latest_mtbi_mechanism" == "vehicle" ~ vehicle_mem_daze,
            dfll$"latest_mtbi_mechanism" == "fall_hit" ~ fall_hit_mem_daze,
            dfll$"latest_mtbi_mechanism" == "violent" ~ violent_mem_daze,
            dfll$"latest_mtbi_mechanism" == "blast" ~ blast_mem_daze,
            dfll$"latest_mtbi_mechanism" == "other_loc" ~ NA_real_,
            dfll$"latest_mtbi_mechanism" == "multi" ~ multi_mem_daze,
            TRUE ~ NA_real_
        ))

    return(dfml)
}
