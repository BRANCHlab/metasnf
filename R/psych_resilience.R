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
        p_family_function, y_family_function, by = "subjectkey")
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
    loneliness <- abcd_import(abcd_ysr01)
    return(stats::na.omit(loneliness))
}

#' Extract screen time
#'
#' @param stq01 ABCD Parent Screen Time Survey
#' @param abcd_stq01 ABCD Youth Screen Time Survey
#' @param subjects Dataframe containing list of required subjects
#'
#' @return screen_time
#'
#' @export
get_screen_time <- function(stq01, abcd_stq01, subjects = NULL) {
    p_screen_time <- abcd_import(stq01)
    y_screen_time <- abcd_import(abcd_stq01)
    screen_time <- dplyr::inner_join(p_screen_time, y_screen_time)
    return(stats::na.omit(screen_time))
}

#' Extract activities
#'
#' @param abcd_saiq02 ABCD Parent Sports and Activities Involvement
#' Questionnaire
#' @param subjects Dataframe containing list of required subjects
#'
#' @return activities
#'
#' @export
get_activities <- function(abcd_saiq02, subjects = NULL) {
    activities <- abcd_import(abcd_saiq02)
    return(stats::na.omit(activities))
}

#' Extract exercise
#'
#' @param abcd_yrb01 ABCD Youth Youth Risk Behavior Survey Exercise Physical
#' Activity
#' @param subjects Dataframe containing list of required subjects
#'
#' @return exercise
#'
#' @export
get_exercise <- function(abcd_yrb01, subjects = NULL) {
    exercise <- abcd_import(abcd_yrb01)
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
    parent_psychopathology <- abcd_import(abcd_asrs01)
    return(stats::na.omit(parent_psychopathology))
}
