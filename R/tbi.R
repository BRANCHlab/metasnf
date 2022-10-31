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
identify_all_tbi <- function(tbi_df) {
    print("Note: This function converts the mTBI column types to numeric.")

    # Assign column types
    dfct <- tbi_df |>
        dplyr::mutate(
            dplyr::across(
                "hosp_er_inj":"other_other_multi_effect_end_age", as.numeric))
    dfct$"interview_age" <- as.numeric(dfct$"interview_age")

    # Generate mtbi and moderate_or_severe_tbi columns
    df_all_tbi <- dfct |> dplyr::mutate(
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
    return(df_all_tbi)
}


#' Generate columns indicating which injury types were mTBIs
#'
#' @param tbi_df A TBI dataframe
#'
#' @return df_mtbi The modified dataframe
#'
#' @export
identify_mtbi <- function(tbi_df) {
    df_mtbi <- tbi_df |> dplyr::mutate(
        hosp_er_mtbi = dplyr::case_when((
                tbi_df$"hosp_er_loc" < 2 &
                tbi_df$"hosp_er_mem_daze" == 1) |
                tbi_df$"hosp_er_loc" == 1 ~ 1,
                TRUE ~ 0),
        vehicle_mtbi = dplyr::case_when((
                tbi_df$"vehicle_loc" < 2 & tbi_df$"vehicle_mem_daze" == 1) |
                tbi_df$"vehicle_loc" == 1 ~ 1,
                TRUE ~ 0),
        fall_hit_mtbi = dplyr::case_when((
                tbi_df$"fall_hit_loc" < 2 & tbi_df$"fall_hit_mem_daze" == 1) |
                tbi_df$"fall_hit_loc" == 1 ~ 1,
                TRUE ~ 0),
        violent_mtbi = dplyr::case_when((
                tbi_df$"violent_loc" < 2 & tbi_df$"violent_mem_daze" == 1) |
                tbi_df$"violent_loc" == 1 ~ 1,
                TRUE ~ 0),
        blast_mtbi = dplyr::case_when((
                tbi_df$"blast_loc" < 2 & tbi_df$"blast_mem_daze" == 1) |
                blast_loc == 1 ~ 1,
                TRUE ~ 0),
        other_loc_mtbi = dplyr::case_when((
                tbi_df$"other_loc_num" - tbi_df$"other_loc_num_over_30") > 0 ~
                    tbi_df$"other_loc_num" - tbi_df$"other_loc_num_over_30",
                TRUE ~ 0),
        multi_mtbi = dplyr::case_when((
                tbi_df$"multi_loc" < 2 & tbi_df$"multi_mem_daze" == 1) |
                tbi_df$"multi_loc" == 1 ~ 1,
                TRUE ~ 0))
    return(df_mtbi)
}

#' Identify time since and age at each mTBI / most recent mTBI
#'
#' @param tbi_df A TBI dataframe
#'
#' @return dfa The modified dataframe
#'
#' @export
identify_mtbi_times <- function(tbi_df) {
    # Scale injury ages to match interview ages if necessary
    if (mean(tbi_df$"hosp_er_age", na.rm = TRUE) < 20) {
        print(paste0("Note: Interview ages are provided in months, but ",
            "injury ages are provided in years. Injury ages will be scaled ",
            "up to determine time between interview and latest mTBI."))
        tbi_df$"blast_age" <-
            tbi_df$"blast_age" * 12
        tbi_df$"hosp_er_age" <-
            tbi_df$"hosp_er_age" * 12
        tbi_df$"vehicle_age" <-
            tbi_df$"vehicle_age" * 12
        tbi_df$"fall_hit_age" <-
            tbi_df$"fall_hit_age" * 12
        tbi_df$"violent_age" <-
            tbi_df$"violent_age" * 12
        tbi_df$"other_loc_min_age" <-
            tbi_df$"other_loc_min_age" * 12
        tbi_df$"multi_effect_end_age" <-
            tbi_df$"multi_effect_end_age" * 12
    }

    # Time since each type of mTBI
    dft <- tbi_df |> dplyr::mutate(
        hosp_er_mtbi_mpi = dplyr::case_when(
            tbi_df$"hosp_er_mtbi" == 1 ~
                tbi_df$"interview_age" - tbi_df$"hosp_er_age"),
        vehicle_mtbi_mpi = dplyr::case_when(
            tbi_df$"vehicle_mtbi" == 1 ~
                tbi_df$"interview_age" - tbi_df$"vehicle_age"),
        fall_hit_mtbi_mpi = dplyr::case_when(
            tbi_df$"fall_hit_mtbi" == 1 ~
                tbi_df$"interview_age" - tbi_df$"fall_hit_age"),
        violent_mtbi_mpi = dplyr::case_when(
            tbi_df$"violent_mtbi" == 1 ~
                tbi_df$"interview_age" - tbi_df$"violent_age"),
        blast_mtbi_mpi = dplyr::case_when(
            tbi_df$"blast_mtbi" == 1 ~
                tbi_df$"interview_age" - tbi_df$"blast_age"),
        other_loc_mtbi_mpi = dplyr::case_when(
            tbi_df$"other_loc_mtbi" == 1 ~
                tbi_df$"interview_age" - tbi_df$"other_loc_min_age"),
        multi_mtbi_mpi = dplyr::case_when(
            tbi_df$"multi_mtbi" == 1 ~
                tbi_df$"interview_age" - tbi_df$"multi_effect_end_age"))

    # Time since latest mTBI
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

    # Age at latest mTBI
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
    return(dfa)
}

#' Add columns to a TBI dataframe indicating mechanism of the latest mTBI
#'
#' @param tbi_df A TBI dataframe
#'
#' @return df_mech The modified dataframe
#'
#' @export
identify_latest_mtbi_mechanism <- function(tbi_df) {
    # Generate column indicating mechanism of latest mTBI
    df_mech <- tbi_df |> dplyr::mutate(latest_mtbi_mechanism = dplyr::case_when(
        tbi_df$"latest_mtbi_age" == tbi_df$"hosp_er_age" ~ "hosp_er",
        tbi_df$"latest_mtbi_age" == tbi_df$"vehicle_age" ~ "vehicle",
        tbi_df$"latest_mtbi_age" == tbi_df$"fall_hit_age" ~ "fall_hit",
        tbi_df$"latest_mtbi_age" == tbi_df$"violent_age" ~ "violent",
        tbi_df$"latest_mtbi_age" == tbi_df$"blast_age" ~ "blast",
        tbi_df$"latest_mtbi_age" == tbi_df$"other_loc_min_age" ~ "other_loc",
        tbi_df$"latest_mtbi_age" == tbi_df$"multi_effect_end_age" ~ "multi"
        ))
    return(df_mech)
}

#' Add columns to a TBI dataframe indicating a subject's estimated mTBI count
#'
#' @param tbi_df A TBI dataframe
#'
#' @return df_num_mtbi The modified dataframe
#'
#' @export
identify_num_mtbi <- function(tbi_df) {
    # Generate column for best guess of number of mTBIs sustained
    df_num_mtbi <- tbi_df |>
        dplyr::mutate(mtbi_count = (
            tbi_df$"hosp_er_mtbi" +
            tbi_df$"vehicle_mtbi" +
            tbi_df$"fall_hit_mtbi" +
            tbi_df$"violent_mtbi" +
            tbi_df$"blast_mtbi" +
            tbi_df$"other_loc_mtbi" +
            tbi_df$"multi_mtbi"))
    return(df_num_mtbi)
}

#' Add columns to a TBI dataframe indicating the LOC of their latest mTBI
#'
#' @param tbi_df A TBI dataframe
#'
#' @return dfll The modified dataframe
#'
#' @export
identify_latest_mtbi_loc <- function(tbi_df) {
    # Create LOC duration variable for the 'other_loc' category
    dfol <- tbi_df |>
        dplyr::mutate(other_loc_loc = dplyr::case_when(
            tbi_df$"other_loc_max_loc_mins" == 0 ~ 0,
            tbi_df$"other_loc_max_loc_mins" > 0 &
                tbi_df$"other_loc_max_loc_mins" <= 30 ~ 1,
            tbi_df$"other_loc_max_loc_mins" > 30 &
                tbi_df$"other_loc_max_loc_mins" <= 1440 ~ 2,
            tbi_df$"other_loc_max_loc_mins" > 1440 ~ 3,
            TRUE ~ NA_real_
        ))

    # Create column indicating loc of latest mTBI
    dfll <- dfol |>
        dplyr::mutate(latest_mtbi_loc = dplyr::case_when(
            dfol$"latest_mtbi_mechanism" == "hosp_er" ~ hosp_er_loc,
            dfol$"latest_mtbi_mechanism" == "vehicle" ~ vehicle_loc,
            dfol$"latest_mtbi_mechanism" == "fall_hit" ~ fall_hit_loc,
            dfol$"latest_mtbi_mechanism" == "violent" ~ violent_loc,
            dfol$"latest_mtbi_mechanism" == "blast" ~ blast_loc,
            dfol$"latest_mtbi_mechanism" == "other_loc" ~ other_loc_loc,
            dfol$"latest_mtbi_mechanism" == "multi" ~ multi_loc,
            TRUE ~ NA_real_
        ))
    return(dfll)
}

#' Add columns to a TBI dataframe indicating if their latest mTBI had mem/daze
#'
#' @param tbi_df A TBI dataframe
#'
#' @return dfmd The modified dataframe
#'
#' @export
identify_latest_mtbi_mem_daze <- function(tbi_df) {
    dfmd <- tbi_df |>
        dplyr::mutate(latest_mtbi_mem_daze = dplyr::case_when(
            tbi_df$"latest_mtbi_mechanism" == "hosp_er" ~ hosp_er_mem_daze,
            tbi_df$"latest_mtbi_mechanism" == "vehicle" ~ vehicle_mem_daze,
            tbi_df$"latest_mtbi_mechanism" == "fall_hit" ~ fall_hit_mem_daze,
            tbi_df$"latest_mtbi_mechanism" == "violent" ~ violent_mem_daze,
            tbi_df$"latest_mtbi_mechanism" == "blast" ~ blast_mem_daze,
            tbi_df$"latest_mtbi_mechanism" == "other_loc" ~ NA_real_,
            tbi_df$"latest_mtbi_mechanism" == "multi" ~ multi_mem_daze,
            TRUE ~ NA_real_
        ))
    return(dfmd)
}
