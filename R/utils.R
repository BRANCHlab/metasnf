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
