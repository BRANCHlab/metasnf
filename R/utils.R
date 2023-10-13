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

#' Convert character-type columns of a dataframe to factor-type
#'
#' @param df A dataframe
#'
#' @return df_converted The dataframe with factor-type columns instead of
#'  char-type columns
#'
#' @export
char_to_fac <- function(df) {
    # Select all the columns that are character type
    char_cols <- df |>
        dplyr::select_if(is.character) |>
        colnames()
    # Convert all those columns to factor type
    for (col in char_cols) {
        df[, col] <- as.factor(df[, col])
    }
    return(df)
}

#' Select all columns of a dataframe not starting with the 'subject_' prefix.
#'
#' @description
#' Removes the 'subject_' prefixed columns from a dataframe. Useful for printing
#'  solutions_matrix structures to the console
#'
#' @param df A dataframe
#'
#' @return df_no_subs Dataframe without subjects
#'
#' @export
no_subs <- function(df) {
    if (!"row_id" %in% colnames(df)) {
        stop("Dataframe requires 'row_id' column.")
    }
    df_no_subs <- df |>
        dplyr::select(
            "row_id",
            !(dplyr::starts_with("subject_"))
        )
    if (identical(df, df_no_subs)) {
        warning("Provided dataframe had no 'subject_' columns to remove.")
    }
    return(df_no_subs)
}

#' Select all columns of a dataframe starting with a given string prefix.
#'
#' @description
#' Removes the columns that are not prefixed with 'subject_' prefixed columns
#'  from a dataframe. Useful intermediate step for extracting subject UIDs from
#'  an solutions_matrix structure.
#'
#' @param df Dataframe
#'
#' @return df_subs Dataframe with only 'subject_' prefixed columns
#'
#' @export
subs <- function(df) {
    if (!"row_id" %in% colnames(df)) {
        stop("Dataframe requires 'row_id' column.")
    }
    df_subs <- df |> dplyr::select(
        "row_id",
        dplyr::starts_with("subject_"))
    if (identical(df, df_subs)) {
        warning("Provided dataframe had no non-'subject_' columns to remove.")
    }
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
#'  (2147483647) multiplied by the threshold.
#'
#' @param train_frac The fraction (0 to 1) of subjects for training
#' @param subjects The available subjects for distribution
#' @param seed Seed used for Jenkins's one_at_a_time hash function
#'
#' @return split a named list containing the training and testing subject_ids
#'
#' @export
train_test_assign <- function(train_frac, subjects, seed = 42) {
    train_thresh <- 2147483647 * train_frac
    train <-
        subjects[abs(digest::digest2int(subjects, seed = seed)) < train_thresh]
    test <-
        subjects[abs(digest::digest2int(subjects, seed = seed)) >= train_thresh]
    if (length(train) == 0 || length(test) == 0) {
        stop("Empty train or test set. Please pick a train_frac closer to 0.5.")
    }
    train_df <- data.frame(subjectkey = train, split = "train")
    test_df <- data.frame(subjectkey = test, split = "test")
    assigned_df <- rbind(train_df, test_df)
    return(assigned_df)
}

#' Filter data to training or testing subjects only
#'
#' @description
#' Given a dataframe and the results of `train_test_split()`, return just the
#'  data for subjects that were assigned the specified split
#'
#' @param df Dataframe to be subsetted into training or testing split
#' @param assigned_df Dataframe containing "subjectkey" and "split" cols from
#'  `train_test_assign()`
#' @param split String indicating which split to keep ("train" or "test")
#' @param old_uid (string) the name of the uid column currently used data
#'
#' @return split_df Dataframe subsetted to specified split
#'
#' @export
keep_split <- function(df, assigned_df, split, old_uid = NULL) {
    # If the UID column of the dataframe is already subjectkey, use it.
    # Otherwise, if the old_uid is in the dataframe, use that.
    # Otherwise, raise error.
    if ("subjectkey" %in% colnames(df)) {
        print("Existing `subjectkey` column will be treated as UID.")
        old_uid <- "subjectkey"
    } else if (is.null(old_uid)) {
        stop("Please provide name of unique ID column using `old_uid`.")
    } else if (!old_uid %in% colnames(df)) {
        stop("Provided `old_uid` parameter is not present in dataframe.")
    } else {
        colnames(assigned_df)[colnames(assigned_df) == "subjectkey"] <- old_uid
    }
    train_or_test <- split
    split_df <- assigned_df |>
        dplyr::filter(split == train_or_test) |>
        #dplyr::inner_join(df, by = "subjectkey") |>
        dplyr::inner_join(df, by = old_uid) |>
        dplyr::select(-split)
    return(split_df)
}

#' Remove items from a data_list or target_list
#'
#' Removes specified elements from a provided data_list or target_list object
#'
#' @param list_object The data_list or target_list containing components to be
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
    list_names <- summarize_target_list(list_object)$"name"
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

#' Time remaining until batch_snf completion
#'
#' @param seconds_per_row Integer in seconds of time taken for most recent SNF
#'  'run
#' @param rows_remaining Number of rows left to complete in the settings matrix
#' @param row Current row in the settings matrix
#' @param remaining_seconds_vector Vector storing up to the 10 most recent
#'  row completion times
#'
#' @return remaining_seconds_vector Updated remaining_seconds_vector
#'
#' @export
batch_snf_time_remaining <- function(seconds_per_row,
                                     rows_remaining,
                                     row,
                                     remaining_seconds_vector) {
    remaining_seconds_vector <- c(remaining_seconds_vector, seconds_per_row)
    if (length(remaining_seconds_vector) > 10) {
        remaining_seconds_vector <-
            remaining_seconds_vector[2:length(remaining_seconds_vector)]
    }
    remaining_seconds <- round(
        mean(remaining_seconds_vector) * rows_remaining, 0
    )
    print(
        paste0(
            "Row: ", row, "/", (row + rows_remaining), " | ",
            "Time remaining: ", remaining_seconds, " seconds"
        )
    )
    return(remaining_seconds_vector)
}

#' Generate a complete path and filename to store an affinity matrix
#'
#' @param affinity_matrix_dir Directory to store affinity matrices
#' @param i Corresponding settings matrix row
#'
#' @return path Complete path and filename to store an affinity matrix
#'
#' @export
affinity_matrix_path <- function(affinity_matrix_dir, i) {
    path <- paste0(
        affinity_matrix_dir,
        "/",
        gsub("-", "_", Sys.Date()), # Today's datej
        "_",
        "affinity_matrix_",
        i,
        ".csv"
    )
    path <- gsub("//", "/", path)
    return(path)
}

#' Helper resample function found in ?sample
#'
#' Like sample, but when given a single value x, returns back that single
#'  value instead of a random value from 1 to x.
#'
#' @param x Vector or single value to sample from
#' @param ... Remaining arguments for base::sample function
#'
#' @export
resample <- function(x, ...) {
    return(x[sample.int(length(x), ...)])
}
