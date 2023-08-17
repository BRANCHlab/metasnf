#' Add columns to a dataframe
#'
#' @description
#' Add new columns to a dataframe by providing a character vector of column
#'  names (param `newcols`) and a value to occupy each row of the new columns
#'  (param `fill`, NA by default)
#'
#' @param df The dataframe to extend
#' @param newcols The vector containing new column names
#' @param fill The values of the elements of the newly added columns. NA by
#'  default.
#'
#' @return extended_df The dataframe containing the added columns
#'
#' @export
add_columns <- function(df, newcols, fill = NA) {
    # ensure that `fill` is not NULL
    if (is.null(fill)) {
        stop("The `fill` parameter must be non-null. Consider NA or \"\".")
    }
    # ensure `newcols` is a character vector
    if (!identical(newcols, as.character(newcols))) {
        warning(
            "`newcols` parameter should be a character vector."
        )
        newcols <- as.character(newcols)
    }
    # generate blank dataframe with the colnames in `newcols`
    newcol_df <- data.frame(t(data.frame(newcols)))
    colnames(newcol_df) <- newcol_df[1, ]
    # populate blank dataframe with fill value
    newcol_df[1, ] <- fill
    # expand original dataframe with `newcol_df`
    extended_df <- dplyr::cross_join(
        df,
        newcol_df,
    )
    return(extended_df)
}

#' Convert dataframe columns to numeric type
#'
#' @description
#' Converts all columns in a dataframe that can be converted to numeric type to
#'  numeric type.
#'
#' @param df A dataframe
#'
#' @return df The dataframe with all possible columns converted to type numeric
#'
#' @export
numcol_to_numeric <- function(df) {
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

#' Training and testing split
#'
#' @description
#' Given a vector of subject_id and a threshold, returns a list of which members
#'  should be in the training set and which should be in the testing set. The
#'  function relies on whether or not the absolute value of the Jenkins's
#'  one_at_a_time hash function exceeds the maximum possible value
#'  (2147483647) multiplied by the threshold
#'
#' @param train_frac The fraction (0 to 1) of subjects for training
#' @param subjects The available subjects for distribution
#'
#' @return split a named list containing the training and testing subject_ids
#'
#' @export
train_test_assign <- function(train_frac, subjects) {
    train_thresh <- 2147483647 * train_frac
    train <-
        subjects[abs(digest::digest2int(subjects, seed = 42)) < train_thresh]
    test <-
        subjects[abs(digest::digest2int(subjects, seed = 42)) >= train_thresh]
    train_df <- data.frame(subjectkey = train, split = "train")
    test_df <- data.frame(subjectkey = test, split = "test")
    assigned_df <- rbind(train_df, test_df)
    return(assigned_df)
}

#' Filter data to training or testing subjects only
#'
#' @description
#' Given a dataframe, assigned_df object (from train_test_split()), and split,
#'  return just the data for subjects that were assigned the specified split
#'
#' @param df Dataframe to be subsetted into training or testing split
#' @param assigned_df Dataframe containing "subjectkey" and "split" cols
#' @param split Split to keep ("train" or "test")
#'
#' @return split_df
#'
#' @export
keep_split <- function(df, assigned_df, split) {
    train_or_test <- split
    split_df <- assigned_df |>
        dplyr::filter(split == train_or_test) |>
        dplyr::inner_join(df, by = "subjectkey") |>
        dplyr::select(-split)
    return(split_df)
}

#' Remove items from a data_list or outcome_list
#'
#' Removes specified elements from a provided data_list or outcome_list object
#'
#' @param list_object The data_list or outcome_list containing components to be
#'  removed
#' @param ... Any number of components to remove from the list object, passed as
#'  strings
#'
#' @return pruned_list The pruned list object
#'
#' @export
list_remove <- function(list_object, ...) {
    to_remove <- list(...)
    # Check to make sure all items to remove are components in list_object
    list_names <- summarize_ol(list_object)$"name"
    invalid_names <- to_remove[!to_remove %in% list_names]
    if (length(invalid_names) > 0) {
        warning(
            paste0(
                "Did you make a typo? The following names are not present in",
                " your data list: ", invalid_names
            )
        )
    }
    pruned_list <- list_object[!list_names %in% to_remove]
    return(pruned_list)
}
