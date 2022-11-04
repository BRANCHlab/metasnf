#' Extract subcortical volumes
#'
#' @param smrip10201 Data file containing subcortical data
#' @param subjects Dataframe containing list of required subjects
#'
#' @return subc_v_df Dataframe of subcortical volumes
#'
#' @export
get_subc_v <- function(smrip10201, subjects) {
    smri_raw <- abcd_import(smrip10201, subjects)
    subc_v_df <- smri_raw |>
        dplyr::select(
            "subjectkey",
            "smri_vol_scs_cbwmatterlh":"smri_vol_scs_subcorticalgv")
    return(stats::na.omit(subc_v_df))
}


#' Extract cortical thicknesses
#'
#' @param mrisdp10201 Data file containing cortical data
#' @param subjects Dataframe containing list of required subjects
#'
#' @return cort_t_df Dataframe of cortical thicknesses
#'
#' @export
get_cort_t <- function(mrisdp10201, subjects) {
    cort_raw <- abcd_import(mrisdp10201, subjects)
    cort_t_df <- cort_raw |>
        dplyr::select(
            "subjectkey",
            "mrisdp_1":"mrisdp_151")
    return(stats::na.omit(cort_t_df))
}


#' Extract cortical surface areas
#'
#' @param mrisdp10201 Data file containing cortical data
#' @param subjects Dataframe containing list of required subjects
#'
#' @return cort_sa_df Dataframe of cortical surface areas
#'
#' @export
get_cort_sa <- function(mrisdp10201, subjects) {
    cort_raw <- abcd_import(mrisdp10201, subjects)
    cort_sa_df <- cort_raw |>
        dplyr::select(
            "subjectkey",
            "mrisdp_303":"mrisdp_453")
    return(stats::na.omit(cort_sa_df))
}


#' Extract white matter neurite densities
#'
#' @param drsip201 Data file containing neurite density data
#' @param subjects Dataframe containing list of required subjects
#'
#' @return wmnd_df Dataframe of white matter neurite densities
#'
#' @export
get_wmnd <- function(drsip201, subjects) {
    nd_raw <- abcd_import(drsip201, subjects)
    wmnd_df <- nd_raw |>
        dplyr::select(
            "subjectkey",
            dplyr::contains("rsirndwm"))
    return(stats::na.omit(wmnd_df))
}


#' Extract cortical network correlations
#'
#' @param betnet02 Data file containing neurite density data
#' @param subjects Dataframe containing list of required subjects
#'
#' @return gord_cor Dataframe of white matter neurite densities
#'
#' @export
get_gord_cor <- function(betnet02, subjects) {
    gord_cor_raw <- abcd_import(betnet02, subjects)
    gord_cor <- gord_cor_raw |>
        dplyr::select(
            "subjectkey",
            "rsfmri_c_ngd_ad_ngd_ad":"rsfmri_c_ngd_vs_ngd_vs")
    return(stats::na.omit(gord_cor))
}


#' Extract subcortical network correlations
#'
#' @param mrirscor02 Data file containing neurite density data
#' @param subjects Dataframe containing list of required subjects
#'
#' @return subc_cor Dataframe of white matter neurite densities
#'
#' @export
get_subc_cor <- function(mrirscor02, subjects) {
    subc_cor_raw <- abcd_import(mrirscor02, subjects)
    subc_cor <- subc_cor_raw |>
        dplyr::select(
            "subjectkey",
            dplyr::contains("rsfmri_cor_ngd_"))
    return(stats::na.omit(subc_cor))
}


#' Extract cortical temporal variances
#'
#' @param mrirstv02 Data file containing neurite density data
#' @param subjects Dataframe containing list of required subjects
#'
#' @return subc_cor Dataframe of white matter neurite densities
#'
#' @export
get_gord_var <- function(mrirstv02, subjects) {
    gord_var_raw <- abcd_import(mrirstv02, subjects)
    gord_var <- gord_var_raw |>
        dplyr::select(
            "subjectkey",
            dplyr::contains("rsfmri_var_cortgordon_"))
    return(stats::na.omit(gord_var))
}

#' Extract cortical temporal variances
#'
#' @param mrirstv02 Data file containing neurite density data
#' @param subjects Dataframe containing list of required subjects
#'
#' @return subc_cor Dataframe of white matter neurite densities
#'
#' @export
get_subc_var <- function(mrirstv02, subjects) {
    subc_var_raw <- abcd_import(mrirstv02, subjects)
    subc_var <- subc_var_raw |>
        dplyr::select(
            "subjectkey",
            dplyr::contains("rsfmri_var_scs_"))
    return(stats::na.omit(subc_var))
}
