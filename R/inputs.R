#' Extract family function
#'
#' @param abcd_fes01
#' @param subjects Dataframe containing list of required subjects
#'
#' @return family_function
#'
#' @export
get_family_function <- function(fes02, abcd_fes01, subjects) {
    p_family_function  <- abcd_import(fes02, subjects)
    y_family_function  <- abcd_import(abcd_fes01, subjects)
    family_function  <- dplyr::inner_join(
        p_family_function, y_family_function, by = "subjectkey")
    return(stats::na.omit(family_function))
}

#' Extract prosocial behaviour
#'
#' @param
#' @param subjects Dataframe containing list of required subjects
#'
#' @return prosocial_behaviour
#'
#' @export
get_prosocial_behaviour <- function(psb01, abcd_psb01, subjects) {
    pr_prosocial <- abcd_import(psb01, subjects)
    yr_prosocial <- abcd_import(abcd_psb01, subjects)
    prosocial <- dplyr::inner_join(
        pr_prosocial, yr_prosocial, by = "subjectkey")
    return(stats::na.omit(prosocial))
}

#' Extract loneliness
#'
#' @param
#' @param subjects Dataframe containing list of required subjects
#'
#' @return loneliness
#'
#' @export
get_loneliness <- function(abcd_ysr01, subjects) {
    loneliness <- abcd_import(abcd_ysr01)
    return(stats::na.omit(loneliness))
}

#' Extract screen time
#'
#' @param
#' @param subjects Dataframe containing list of required subjects
#'
#' @return screen_time
#'
#' @export
get_screen_time <- function(stq01, abcd_stq01, subjects) {
    p_screen_time <- abcd_import(stq01)
    y_screen_time <- abcd_import(abcd_stq01)
    screen_time <- dplyr::inner_join(p_screen_time, y_screen_time)
    return(stats::na.omit(screen_time))
}

#' Extract activities
#'
#' @param
#' @param subjects Dataframe containing list of required subjects
#'
#' @return activities
#'
#' @export
get_activities <- function(abcd_saiq02, subjects) {
    activities <- abcd_import(abcd_saiq02)
    return(stats::na.omit(activities))
}

#' Extract exercise
#'
#' @param
#' @param subjects Dataframe containing list of required subjects
#'
#' @return exercise
#'
#' @export
get_exercise <- function(abcd_yrb01, subjects) {
    exercise <- abcd_import(abcd_yrb01)
    return(stats::na.omit(exercise))
}

#' Extract parent psychopathology
#'
#' @param
#' @param subjects Dataframe containing list of required subjects
#'
#' @return parent_psychopathology
#'
#' @export
get_parent_psychopathology <- function(abcd_asrs01, subjects) {
    parent_psychopathology <- abcd_import(abcd_asrs01)
    return(stats::na.omit(parent_psychopathology))
}
