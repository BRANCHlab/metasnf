#' Extract family function
#'
#' @param fes02 ABCD Parent Family Environment Scale-Family Conflict Subscale
#' Modified from PhenX
#' @param abcd_fes01 ABCD Parent Family Environment Scale-Family Conflict
#' Subscale Modified from PhenX
#' @param subjects Dataframe containing list of required subjects
#'
#' @return family_function
#'
#' @export
get_family_function <- function(fes02, abcd_fes01, subjects = NULL) {
    p_family_function  <- abcd_import(fes02, subjects)
    y_family_function  <- abcd_import(abcd_fes01, subjects)
    family_function  <- dplyr::inner_join(
        p_family_function, y_family_function, by = "subjectkey") |>
    dplyr::select(
        "subjectkey",
        "fes_youth_q1",
        "fes_youth_q2",
        "fes_youth_q3",
        "fes_youth_q4",
        "fes_youth_q5",
        "fes_youth_q6",
        "fes_youth_q7",
        "fes_youth_q8",
        "fes_youth_q9",
        "fam_enviro1_p",
        "fam_enviro2r_p",
        "fam_enviro3_p",
        "fam_enviro4r_p",
        "fam_enviro5_p",
        "fam_enviro6_p",
        "fam_enviro7r_p",
        "fam_enviro8_p",
        "fam_enviro9r_p")
    # Convert columns to numeric for subsequent averaging
    family_function <- col_to_num(family_function, 2:length(family_function))
    # Average the reports from youth and parents
    family_function <- family_function |>
        dplyr::mutate(
            "q1_fight" = family_function$"fes_youth_q1" +
                family_function$"fam_enviro1_p",
            "q2_angry" = family_function$"fes_youth_q2" +
                family_function$"fam_enviro2r_p",
            "q3_throw" = family_function$"fes_youth_q3" +
                family_function$"fam_enviro3_p",
            "q4_temper" = family_function$"fes_youth_q4" +
                family_function$"fam_enviro4r_p",
            "q5_criticize" = family_function$"fes_youth_q5" +
                family_function$"fam_enviro5_p",
            "q6_hit" = family_function$"fes_youth_q6" +
                family_function$"fam_enviro6_p",
            "q7_peaceful" = family_function$"fes_youth_q7" +
                family_function$"fam_enviro7r_p",
            "q8_outdo" = family_function$"fes_youth_q8" +
                family_function$"fam_enviro8_p",
            "q9_yell" = family_function$"fes_youth_q9" +
                family_function$"fam_enviro9r_p") |>
        dplyr::select("subjectkey", dplyr::starts_with("q"))
    return(stats::na.omit(family_function))
}

#' Extract prosocial behaviour
#'
#' @param psb01 Parent Prosocial Behavior Survey
#' @param abcd_psb01 Youth Prosocial Behavior Survey
#' @param subjects Dataframe containing list of required subjects
#'
#' @return prosocial_behaviour
#'
#' @export
get_prosocial_behaviour <- function(psb01, abcd_psb01, subjects = NULL) {
    pr_prosocial <- abcd_import(psb01, subjects)
    yr_prosocial <- abcd_import(abcd_psb01, subjects)
    prosocial <- dplyr::inner_join(
        pr_prosocial, yr_prosocial, by = "subjectkey")
    prosocial <- prosocial |>
        dplyr::select("subjectkey",
                      dplyr::ends_with(c("_y", "_p")))
    prosocial <- col_to_num(prosocial, 2:length(prosocial))
    prosocial <- prosocial |>
        dplyr::mutate(
            "q1" = prosocial$"prosocial_q1_y" + prosocial$"prosocial_q1_p",
            "q2" = prosocial$"prosocial_q2_y" + prosocial$"prosocial_q2_p",
            "q3" = prosocial$"prosocial_q3_y" + prosocial$"prosocial_q3_p") |>
        dplyr::select("subjectkey", dplyr::starts_with("q"))
    return(stats::na.omit(prosocial))
}

#' Extract loneliness
#'
#' @param abcd_ysr01 ABCD Other Resilience
#' @param subjects Dataframe containing list of required subjects
#'
#' @return loneliness
#'
#' @export
get_loneliness <- function(abcd_ysr01, subjects = NULL) {
    loneliness <- abcd_import(abcd_ysr01, subjects) |>
        dplyr::select(
            "subjectkey",
            "sex",
            "resiliency5a_y",
            "resiliency5b_y",
            "resiliency6a_y",
            "resiliency6b_y") |>
        dplyr::rename(
            "friend_boy" = "resiliency5a_y",
            "close_friend_boy" = "resiliency5b_y",
            "friend_girl" = "resiliency6a_y",
            "close_friend_girl" = "resiliency6b_y")
    loneliness <- loneliness |>
        dplyr::mutate(
            "ss_friend" = dplyr::case_when(
                loneliness$"sex" == "M" ~
                    as.numeric(loneliness$"friend_boy"),
                loneliness$"sex" == "F" ~
                    as.numeric(loneliness$"friend_girl")),
            "os_friend" = dplyr::case_when(
                loneliness$"sex" == "M" ~
                    as.numeric(loneliness$"friend_girl"),
                loneliness$"sex" == "F" ~
                    as.numeric(loneliness$"friend_boy")),
            "ss_close_friend" = dplyr::case_when(
                loneliness$"sex" == "M" ~
                    as.numeric(loneliness$"close_friend_boy"),
                loneliness$"sex" == "F" ~
                    as.numeric(loneliness$"close_friend_girl")),
            "os_close_friend" = dplyr::case_when(
                loneliness$"sex" == "M" ~
                    as.numeric(loneliness$"close_friend_girl"),
                loneliness$"sex" == "F" ~
                    as.numeric(loneliness$"close_friend_boy"))) |>
        dplyr::select(
            "subjectkey",
            "ss_friend",
            "os_friend",
            "ss_close_friend",
            "os_close_friend")
    return(stats::na.omit(loneliness))
}

#' Extract healthy behaviours: screen time questionnaire
#'
#' @param stq01 ABCD Parent Screen Time Survey
#' @param subjects Dataframe containing list of required subjects
#'
#' @return screen_time
#'
#' @export
get_screen_time <- function(stq01, subjects = NULL) {
    screen_time <- abcd_import(stq01, subjects) |>
        dplyr::select(
            "subjectkey",
            "screentime1_p_hours",
            "screentime1_p_minutes",
            "screentime2_p_hours",
            "screentime2_p_minutes")
    # Convert columns to numeric
    char_cols <- colnames(screen_time)[2:length(screen_time)]
    screen_time[char_cols] <- sapply(screen_time[char_cols], as.numeric)
    # Convert to hours
    screen_time <- screen_time |>
        dplyr::mutate(
            "screentime_wknd_hrs" = screen_time$"screentime1_p_hours" +
                (screen_time$"screentime1_p_minutes" / 60),
            "screentime_wkday_hrs" = screen_time$"screentime2_p_hours" +
                (screen_time$"screentime2_p_minutes" / 60)) |>
        dplyr::select(dplyr::contains(c("subjectkey", "wknd", "wkday")))
    return(stats::na.omit(screen_time))
}

#' Extract healthy behaviours: spots and activities questionnaire
#'
#' @param abcd_saiq02 ABCD Parent Sports and Activities Involvement
#' Questionnaire
#' @param subjects Dataframe containing list of required subjects
#'
#' @return activities
#'
#' @export
get_sports_and_activities <- function(abcd_saiq02, subjects = NULL) {
    sports <- abcd_import(abcd_saiq02, subjects)
    sports <- sports |> dplyr::select("subjectkey", dplyr::starts_with("sai"))
    sports <- col_to_num(sports, 2:length(sports))
    # Number of activities organized inside of school
    org_school_cols <- colnames(sports)[endsWith(colnames(sports), "school")]
    sports$"organized_school_activities" <-
        rowSums(sports[, org_school_cols], na.rm = TRUE)
    # Number of activities organized outside of school
    org_out_cols <- colnames(sports)[endsWith(colnames(sports), "outside")]
    sports$"organized_outside_activities" <-
        rowSums(sports[, org_out_cols], na.rm = TRUE)
    # Number of activities receiving private instruction
    private_cols <- colnames(sports)[endsWith(colnames(sports), "private")]
    sports$"private_instruction_activities" <-
        rowSums(sports[, private_cols], na.rm = TRUE)
    # Number of activities participated in the last year
    p12_cols <- colnames(sports)[endsWith(colnames(sports), "p12")]
    sports$"p12_activities" <-
        rowSums(sports[, p12_cols], na.rm = TRUE)
    # Select columns
    sports <- sports |>
        dplyr::select("subjectkey", dplyr::ends_with("activities"))
    return(stats::na.omit(sports))
}

#' Extract healthy behaviours: exercise questionnaire
#'
#' @param abcd_yrb01 ABCD Youth Youth Risk Behavior Survey Exercise Physical
#' Activity
#' @param subjects Dataframe containing list of required subjects
#'
#' @return exercise
#'
#' @export
get_exercise <- function(abcd_yrb01, subjects = NULL) {
    exercise <- abcd_import(abcd_yrb01, subjects) |>
        dplyr::select("subjectkey", dplyr::ends_with("y"))
    return(stats::na.omit(exercise))
}

#' Extract parent psychopathology
#'
#' @param abcd_asrs01 ABCD Parent Adult Self Report Scores Aseba
#' @param subjects Dataframe containing list of required subjects
#'
#' @return parent_psychopathology
#'
#' @export
get_parent_psychopathology <- function(abcd_asrs01, subjects = NULL) {
    parent_psychopathology <- abcd_import(abcd_asrs01, subjects) |>
        dplyr::select("subjectkey", dplyr::ends_with("r"))
    return(stats::na.omit(parent_psychopathology))
}
