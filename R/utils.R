#' Add character vector as columns
#'
#' @description
#' Extend a dataframe with columns from a character vector
#'
#' @param df The dataframe to extend
#' @param char_vector The vector containing new column names
#' @param filler The values of the elements of the newly added columns
#'
#' @return extended_df The dataframe containing the added columns
#'
#' @export
add_char_vec_as_cols <- function(df, char_vector, filler) {
    newcols <- data.frame(t(data.frame(char_vector)))
    colnames(newcols) <- newcols[1, ]
    newcols[1, ] <- filler
    extended_df <- dplyr::inner_join(
        df,
        newcols,
        by = character()
    )
    return(extended_df)
}

#' Select all columns of a dataframe not starting with a given string prefix.
#'
#' @description
#' Originally constructed to return a dataframe without any subject columns.
#'  Returns a dataframe excluding all columns with a specified string prefix.
#'
#' @param df Datframe
#'
#' @return df_no_subs Dataframe without subjects
#'
#' @export
no_subs <- function(df) {
    df_no_subs <- df |> dplyr::select(!(dplyr::starts_with("NDAR")))
    return(df_no_subs)
}

#' Select all columns of a dataframe starting with a given string prefix.
#'
#' @description
#' Originally constructed to return a dataframe with only subject columns.
#'  Returns a dataframe including only columns with a specified string prefix.
#'
#' @param df Dataframe
#'
#' @return df_subs Dataframe without subjects
#'
#' @export
subs <- function(df) {
    df_subs <- df |> dplyr::select(
        "row_id",
        dplyr::starts_with("NDAR"))
    return(df_subs)
}

