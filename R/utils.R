#' Remove the data dictionary from a dataset
#'
#' @param abcd_df An ABCD dataframe containing a data dictionary
#'
#' @return abcd_df_no_dd The same dataframe without the data dictionary
#'
#' @export
remove_dd <- function(abcd_df) {
    abcd_df_no_dd <- abcd_df[-1, ]
    return(abcd_df_no_dd)
}


#' Remove the data dictionary from a dataset
#'
#' @param abcd_df An ABCD dataframe containing a data dictionary
#'
#' @return baseline_abcd_df Baseline data only
#'
#' @export
filter_baseline <- function(abcd_df) {
    baseline_abcd_df <- abcd_df |>
        dplyr::filter(abcd_df$"eventname" == "baseline_year_1_arm_1")
    return(baseline_abcd_df)
}


#' Subset a dataframe to a given subject list
#'
#' @param abcd_df An ABCD dataframe
#' @param subjects Dataframe containing list of required subjects
#'
#' @return filtered_df The subsetted dataframe
#'
#' @export
filter_subjects <- function(abcd_df, subjects = NULL) {
    if (is.null(subjects)) {
        return(abcd_df)
    } else {
        filtered_df <- dplyr::inner_join(abcd_df, subjects, by = "subjectkey")
        return(filtered_df)
    }
}


#' Import a raw ABCD dataframe for selected subjects at baseline
#'
#' @param abcd_df An ABCD dataframe
#' @param subjects Dataframe containing list of required subjects
#'
#' @return abcd_clean_df The subsetted dataframe
#'
#' @export
abcd_import <- function(abcd_df, subjects = NULL) {
    abcd_clean_df <- abcd_df |>
        remove_dd() |>
        filter_baseline() |>
        filter_subjects(subjects)
    abcd_clean_df <- abcd_clean_df |>
        dplyr::arrange(abcd_clean_df$"subjectkey")
    return(abcd_clean_df)
}

dummy <- function(abcd_df, cols) {
    abcd_df <- fastDummies::dummy_cols(
        .data = abcd_df,
        select_columns = cols,
        remove_selected_columns = TRUE)
    return(abcd_df)
}
