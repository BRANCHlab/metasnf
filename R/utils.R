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

#' Convert all possible columns to numeric
#'
#' @param df A dataframe or tibble
#'
#' @return df The same dataframe with all possible columns made numeric
#'
#' @export
col_to_num_all_possible <- function(df) {
    df[] <- lapply(df,
        function(x) {
            tryCatch({
                return(as.numeric(x))
            },
            warning = function(cond) {
                if (cond$"message" == "NAs introduced by coercion")
                return(x)
            })
        })
    return(df)
}

#' Convert char columns to factors
#'
#' @param df The dataframe containing char columns to be converted
#'
#' @return df The dataframe with factor columns
#'
#' @export
char_to_fac <- function(df) {
    df[sapply(df, is.character)] <-
        lapply(df[sapply(df, is.character)], as.factor)
    return(df)
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
    df_no_subs <- df |> dplyr::select(!(dplyr::starts_with("subject_")))
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
        dplyr::starts_with("subject_"))
    return(df_subs)
}

#' Merge list of dataframes
#'
#' @param df_list list of dataframes
#' @param join String indicating if join should be "inner" or "full"
#'
#' @return merged_df inner join of all dataframes in list
#'
#' @export
merge_df_list <- function(df_list, join = "inner") {
    if (join == "inner") {
        merged_df <- df_list |>
            purrr::reduce(dplyr::inner_join, by = "subjectkey")
    } else if (join == "full") {
        merged_df <- df_list |>
            purrr::reduce(dplyr::full_join, by = "subjectkey")
    } else {
        print("Invalid join type specified. Options are 'inner' and 'full'.")
        return(NULL)
    }
    return(merged_df)
}


