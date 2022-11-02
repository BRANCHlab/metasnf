#' Select columns relevant for demographic inputs
#'
#' @param youth_pubertal_df x
#' @param parent_pubertal_df x
#'
#' @return pubertal_status x
#'
#' @export
get_pubertal_status <- function(youth_pubertal_df, parent_pubertal_df) {
    # Merge parent and youth dataframes
    pubertal_df_full <- dplyr::inner_join(youth_pubertal_df, parent_pubertal_df,
        by = c("subjectkey", "sex"))
    # Assign proper column types
    pubertal_df_full$"pds_y_ss_female_category" <-
        as.numeric(pubertal_df_full$"pds_y_ss_female_category")
    pubertal_df_full$"pds_p_ss_female_category" <-
        as.numeric(pubertal_df_full$"pds_p_ss_female_category")
    pubertal_df_full$"pds_y_ss_male_category" <-
        as.numeric(pubertal_df_full$"pds_y_ss_male_category")
    pubertal_df_full$"pds_p_ss_male_category" <-
        as.numeric(pubertal_df_full$"pds_p_ss_male_category")
    # Composite pubertal status by averaging parent and youth reports
    pubertal_df_full$pubertal_status <- rowMeans(pubertal_df_full[,
        c("pds_y_ss_female_category",
          "pds_p_ss_female_category",
          "pds_y_ss_male_category",
          "pds_p_ss_male_category")], na.rm = TRUE)
    # Select relevant variables
    pubertal_status <- pubertal_df_full |>
        dplyr::select(
            "subjectkey",
            "pubertal_status")
    return(pubertal_status)
}


#' Returns combined household incomes split into low, medium, and high groups
#'
#' Low: $0 - $50k, Medium: $50k - $100k, High: > $100k
#'
#' @param parent_demographics x
#'
#' @return income_df x
#'
#' @export
get_income <- function(parent_demographics) {
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
    return(income_df)
}


#' Returns race information
#'
#' @param parent_demographics x
#'
#' @return race_df x
#'
#' @export
get_race <- function(parent_demographics) {
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
               "hispanic" = "demo_ethn_v2")
    # Fix the hispanic category
    race_df <- race_df |>
        dplyr::mutate(hispanic = dplyr::case_when(race_df$"hispanic" == 1 ~ 1,
                                    TRUE ~ 0))
    # Assign proper column types
    race_df <- race_df |>
        dplyr::mutate(
            dplyr::across(
                "white":"hispanic", as.numeric))
    # Based on known frequencies of races among subject list, pool groups
    race_df <- race_df |> dplyr::mutate(
        asian =
            race_df$"asian" +
            race_df$"filipino" +
            race_df$"korean" +
            race_df$"other_asian" +
            race_df$"chinese" +
            race_df$"japanese" +
            race_df$"vietnamese",
        other =
            pmax(
            race_df$"native_american",
            race_df$"samoan",
            race_df$"guamanian",
            race_df$"other_pacific_islander",
            race_df$"native_hawaiian",
            race_df$"native_alaskan",
            race_df$"other"),
        na =
            race_df$"dont_know" +
            race_df$"refuse_to_answer") |>
        dplyr::select("subjectkey",
               "asian",
               "other",
               "na",
               "white",
               "black",
               "hispanic")
    # Assign mixed race for those in multiple categories
    race_df <- race_df |>
        dplyr::mutate(mixed = dplyr::case_when(
            race_df$"white" +
            race_df$"black" +
            race_df$"asian" +
            race_df$"other" +
            race_df$"na" +
            race_df$"hispanic" > 1 ~ 1,
            TRUE ~ 0
            ))
    # Remove original race category for those who are mixed
    race_df <- race_df |>
        dplyr::mutate(
            black = dplyr::case_when(
                race_df$"black" == 1 & race_df$"mixed" == 0 ~ 1,
                TRUE ~ 0),
            white = dplyr::case_when(
                race_df$"white" == 1 & race_df$"mixed" == 0 ~ 1,
                TRUE ~ 0),
            asian = dplyr::case_when(
                race_df$"asian" == 1 & race_df$"mixed" == 0 ~ 1,
                TRUE ~ 0),
            hispanic = dplyr::case_when(
                race_df$"hispanic" == 1 & race_df$"mixed" == 0 ~ 1,
                TRUE ~ 0))
    # Pool together mixed / other race
    # As only a very small number of asian & hispanic subjects are non-mixed,
    # pool them in as well.
    race_df <- race_df |>
        dplyr::mutate(
            mixed_or_other = dplyr::case_when(
                race_df$"asian" == 1 ~ 1,
                race_df$"hispanic" == 1 ~ 1,
                race_df$"mixed" == 1 ~ 1,
                race_df$"other" == 1 ~ 1,
                TRUE ~ 0)) |>
        dplyr::filter(race_df$"na" == 0) |>
        dplyr::select("subjectkey",
                      "white",
                      "black",
                      "mixed_or_other")
    # Undummy the dataframe
    race_df <- race_df |>
        dplyr::mutate(
            race = dplyr::case_when(
                race_df$"white" == 1 ~ "white",
                race_df$"black" == 1 ~ "black",
                race_df$"mixed_or_other" == 1 ~ "mixed_or_other",
            )) |>
        dplyr::select("subjectkey", "race")
    race_df <- race_df |>
        dplyr::filter(!(is.na(race_df$"race")))
    return(race_df)
}


#' Returns race information
#'
#' @param puberty x
#' @param age x
#' @param sex x
#' @param income x
#' @param race x
#'
#' @return race_df x
#'
#' @export
generate_demographic <- function(puberty, age, sex, income, race) {
    merge_1 <- dplyr::inner_join(puberty, age, by = "subjectkey")
    merge_2 <- dplyr::inner_join(sex, income, by = "subjectkey")
    merge_3 <- dplyr::inner_join(merge_1, merge_2, by = "subjectkey")
    demographic_df <- dplyr::inner_join(merge_3, race, by = "subjectkey")
    return(demographic_df)
}
