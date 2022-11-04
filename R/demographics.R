#' Return dataframe containing pubertal status of specified subjects
#'
#' @param ssphp01 Dataframe containing parent pubertal status report
#' @param ssphy01 Dataframe containing youth pubertal status report
#' @param subjects Dataframe containing list of required subjects
#'
#' @return pubertal_status Dataframe containing average pubertal status
#'
#' @export
get_pubertal_status <- function(ssphp01, ssphy01, subjects) {
    youth_pubertal_df <- abcd_import(ssphy01, subjects)
    parent_pubertal_df <- abcd_import(ssphp01, subjects)
    # Merge parent and youth dataframes
    puberty_full <- dplyr::inner_join(youth_pubertal_df, parent_pubertal_df,
        by = c("subjectkey", "sex"))
    # Assign proper column types
    puberty_full$"pds_y_ss_female_category" <-
        as.numeric(puberty_full$"pds_y_ss_female_category")
    puberty_full$"pds_p_ss_female_category" <-
        as.numeric(puberty_full$"pds_p_ss_female_category")
    puberty_full$"pds_y_ss_male_category" <-
        as.numeric(puberty_full$"pds_y_ss_male_category")
    puberty_full$"pds_p_ss_male_category" <-
        as.numeric(puberty_full$"pds_p_ss_male_category")
    # Composite pubertal status by averaging parent and youth reports
    puberty_full$pubertal_status <- rowMeans(puberty_full[,
        c("pds_y_ss_female_category",
          "pds_p_ss_female_category",
          "pds_y_ss_male_category",
          "pds_p_ss_male_category")], na.rm = TRUE)
    # Select relevant variables
    pubertal_status <- puberty_full |>
        dplyr::select(
            "subjectkey",
            "pubertal_status")
    return(stats::na.omit(pubertal_status))
}

#' Returns combined household incomes split into low, medium, and high groups
#'
#' Low: $0 - $50k, Medium: $50k - $100k, High: > $100k
#'
#' @param pdem02 Dataframe containing parent demographic information
#' @param subjects Dataframe containing list of required subjects
#'
#' @return income_df Dataframe containing household incomes
#'
#' @export
get_income <- function(pdem02, subjects) {
    parent_demographics <- abcd_import(pdem02, subjects)
    parent_demographics$"demo_comb_income_v2" <-
        as.numeric(parent_demographics$"demo_comb_income_v2")
    income_df <- parent_demographics |>
        dplyr::select(
            "subjectkey",
            "demo_comb_income_v2")
    income_df <- income_df |>
        dplyr::mutate(household_income = dplyr::case_when(
            parent_demographics$"demo_comb_income_v2" == 777 ~ NA_real_,
            parent_demographics$"demo_comb_income_v2" == 999 ~ NA_real_,
            parent_demographics$"demo_comb_income_v2" < 7 ~ 1,
            parent_demographics$"demo_comb_income_v2" < 9 ~ 2,
            parent_demographics$"demo_comb_income_v2" < 11 ~ 3,
            TRUE ~ NA_real_,
            ))
    income_df <- income_df |>
        dplyr::select("subjectkey", "household_income") |>
        dplyr::filter(!(is.na(income_df$"household_income")))
    return(stats::na.omit(income_df))
}

#' Returns race information
#'
#' @param pdem02 Dataframe containing parent demographic information
#' @param subjects Dataframe containing list of required subjects
#' @param format String indicating format to output race data
#'
#' @return race_df Dataframe containing subject race
#'
#' @export
get_race <- function(pdem02, subjects, format = "") {
    options <- c("condensed_dummied",
                 "condensed_undummied",
                 "expanded_dummied")
    if (!(format %in% options)) {
        print("The 'format argument should be one of the following options:")
        print("[1] 'condensed_dummied'")
        print("[2] 'condensed_undummied'")
        print("[3] 'expanded_dummied'")
        print("See ?get_race for more information about these options.")
        return(NULL)
    }
    parent_demographics <- abcd_import(pdem02, subjects)
    # Rename columns
    race_df <- parent_demographics |>
        dplyr::rename("white" = "demo_race_a_p___10",
               "black" = "demo_race_a_p___11",
               "native_american" = "demo_race_a_p___12",
               "native_alaskan" = "demo_race_a_p___13",
               "native_hawaiian" = "demo_race_a_p___14",
               "guamanian" = "demo_race_a_p___15",
               "samoan" = "demo_race_a_p___16",
               "other_pacific_islander" = "demo_race_a_p___17",
               "asian" = "demo_race_a_p___18",
               "chinese" = "demo_race_a_p___19",
               "filipino" = "demo_race_a_p___20",
               "japanese" = "demo_race_a_p___21",
               "korean" = "demo_race_a_p___22",
               "vietnamese" = "demo_race_a_p___23",
               "other_asian" = "demo_race_a_p___24",
               "other" = "demo_race_a_p___25",
               "refuse_to_answer" = "demo_race_a_p___77",
               "dont_know" = "demo_race_a_p___99",
               "hispanic" = "demo_ethn_v2") |>
    # Select relevant variables
    dplyr::select("subjectkey",
                  "white":"hispanic",
                  -"demo_race_notes_v2") |>
    # Assign numeric column types to help with later transformations
    dplyr::mutate(
        dplyr::across(
            "white":"hispanic", as.numeric))
    # Fix the hispanic category
    race_df <- race_df |>
        dplyr::mutate(hispanic = dplyr::case_when(race_df$"hispanic" == 1 ~ 1,
                                    TRUE ~ 0)) |>
    # Based on known frequencies of races among subject list, pool groups
    dplyr::mutate(asian_other_pi = pmax(
        race_df$"asian",
        race_df$"filipino",
        race_df$"korean",
        race_df$"other_asian",
        race_df$"chinese",
        race_df$"japanese",
        race_df$"vietnamese",
        race_df$"other_pacific_islander",
        race_df$"samoan",
        race_df$"guamanian",
        race_df$"native_hawaiian",
        race_df$"native_alaskan"),
    na = pmax(
        race_df$"dont_know",
        race_df$"refuse_to_answer")) |>
    dplyr::select("subjectkey",
           "asian_other_pi",
           "native_american",
           "other",
           "na",
           "white",
           "black",
           "hispanic")
    # three columns of white, black, mixed/other
    if (format == "condensed_dummied" || format == "condensed_undummied") {
        # Assign mixed race for those in multiple categories
        race_df <- race_df |>
            dplyr::mutate(mixed = dplyr::case_when(
                race_df$"white" +
                race_df$"black" +
                race_df$"asian_other_pi" +
                race_df$"native_american" +
                race_df$"other" +
                race_df$"hispanic" > 1 ~ 1,
                TRUE ~ 0
                ))
        # Remove original race category for those who are mixed
        race_df <- race_df |>
            dplyr::mutate(
                black = dplyr::case_when(
                    race_df$"black" == 1 &
                        race_df$"mixed" == 0 ~ 1,
                    TRUE ~ 0),
                white = dplyr::case_when(
                    race_df$"white" == 1 &
                        race_df$"mixed" == 0 ~ 1,
                    TRUE ~ 0),
                asian_other_pi = dplyr::case_when(
                    race_df$"asian_other_pi" == 1 &
                        race_df$"mixed" == 0 ~ 1,
                    TRUE ~ 0),
                hispanic = dplyr::case_when(
                    race_df$"hispanic" == 1 &
                        race_df$"mixed" == 0 ~ 1,
                    TRUE ~ 0),
                native_american = dplyr::case_when(
                    race_df$"native_american" == 1 &
                        race_df$"mixed" == 0 ~ 1,
                    TRUE ~ 0))
        # Pool together mixed / other race. As only a very small number of
        # asian & hispanic subjects are non-mixed, pool them in as well.
        race_df <- race_df |>
            dplyr::mutate(
                mixed_or_other = dplyr::case_when(
                    race_df$"asian_other_pi" == 1 ~ 1,
                    race_df$"hispanic" == 1 ~ 1,
                    race_df$"mixed" == 1 ~ 1,
                    race_df$"other" == 1 ~ 1,
                    TRUE ~ 0)) |>
            dplyr::select("subjectkey",
                          "white",
                          "black",
                          "mixed_or_other")
    }
    if (format == "condensed_undummied") {
        # Undummy the dataframe
        race_df <- race_df |>
            dplyr::mutate(
                race = dplyr::case_when(
                    race_df$"white" == 1 ~ "white",
                    race_df$"black" == 1 ~ "black",
                    race_df$"mixed_or_other" == 1 ~ "mixed_or_other",
                )) |>
            dplyr::select("subjectkey", "race")
    }
    return(stats::na.omit(race_df))
}

#' Return dataframe containing interview age of specified subjects
#'
#' @param abcd_df Any ABCD dataframe containing interview age
#' @param subjects Dataframe containing list of required subjects
#'
#' @return interview_age Dataframe containing interview age
#'
#' @export
get_interview_age <- function(abcd_df, subjects) {
    interview_age <- abcd_import(abcd_df, subjects) |>
        dplyr::select("subjectkey", "interview_age")
    return(stats::na.omit(interview_age))
}

#' Return dataframe containing sex of specified subjects
#'
#' @param abcd_df Any ABCD dataframe containing sex
#' @param subjects Dataframe containing list of required subjects
#'
#' @return sex Dataframe containing sex
#'
#' @export
get_sex <- function(abcd_df, subjects) {
    sex <- abcd_import(abcd_df, subjects) |>
        dplyr::select("subjectkey", "sex")
    return(stats::na.omit(sex))
}

#' Get acute symptom input variable 'latest_mtbi_age'
#'
#' @param otbi01 The baseline TBI dataframe
#' @param subjects Dataframe containing list of required subjects
#'
#' @return mtbi_age Dataframe containing latest_mtbi_age
#'
#' @export
get_mtbi_age <- function(otbi01, subjects) {
    mtbi_age <- detail_mtbi(otbi01, subjects) |>
        dplyr::select(
            "subjectkey",
            "latest_mtbi_age"
        )
    return(stats::na.omit(mtbi_age))
}
