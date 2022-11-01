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
#' @param subject_list A dataframe containing subjects in the 'subjectkey' col
#'
#' @return filtered_df The subsetted dataframe
#'
#' @export
filter_subjects <- function(abcd_df, subject_list) {
    filtered_df <- dplyr::inner_join(abcd_df, subject_list, by = "subjectkey")
    return(filtered_df)
}
