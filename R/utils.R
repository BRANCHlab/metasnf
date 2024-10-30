#' Add columns to a dataframe
#'
#' Add new columns to a dataframe by providing a character vector of column
#'  names (param `newcols`) and a value to occupy each row of the new columns
#'  (param `fill`, NA by default).
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
            tryCatch(
                {
                    return(as.numeric(x))
                }, warning = function(cond) {
                    if (cond$"message" == "NAs introduced by coercion") {
                        return(x)
                    }
                }
            )
        }
    )
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
#' Removes the 'subject_' prefixed columns from a dataframe. Useful for printing
#'  solutions_matrix structures to the console.
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
        dplyr::starts_with("subject_")
    )
    if (identical(df, df_subs)) {
        warning("Provided dataframe had no non-'subject_' columns to remove.")
    }
    return(df_subs)
}

#' Merge list of dataframes
#'
#' @param df_list list of dataframes
#'
#' @param join String indicating if join should be "inner" or "full"
#'
#' @param uid Column name to join on. Default is "subjectkey"
#'
#' @param no_na Whether to remove NA values from the merged dataframe
#'
#' @return merged_df inner join of all dataframes in list
#'
#' @export
merge_df_list <- function(df_list,
                          join = "inner",
                          uid = "subjectkey",
                          no_na = FALSE) {
    if (join == "inner") {
        merged_df <- df_list |> purrr::reduce(
            dplyr::inner_join,
            by = uid
        )
    } else if (join == "full") {
        merged_df <- df_list |> purrr::reduce(
            dplyr::full_join,
            by = uid
        )
    } else {
        stop("Invalid join type specified. Options are 'inner' and 'full'.")
    }
    if (no_na) {
        merged_df <- stats::na.omit(merged_df)
    }
    return(merged_df)
}

#' Pull complete-data UIDs from a list of dataframes
#'
#' @param list_of_dfs List of dataframes.
#'
#' @param uid Name of column across dataframes containing UIDs
#'
#' @return A character vector of the UIDs of observations that have complete
#' data across the provided list of dataframes.
#'
#' @export
get_complete_uids <- function(list_of_dfs, uid) {
    merged_df <- merge_df_list(
        list_of_dfs,
        join = "inner",
        uid = uid,
        no_na = TRUE
    ) 
    complete_uids <- merged_df[, uid] |>
        data.frame() |>
        unlist() |>
        as.character()
    return(complete_uids)
}

#' Training and testing split
#'
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
    hash <- abs(digest::digest2int(subjects, seed = seed))
    train <- subjects[hash < train_thresh]
    test <- subjects[hash >= train_thresh]
    assigned_subs <- list(train = train, test = test)
    if (length(train) == 0 || length(test) == 0) {
        warning("Empty train or test set.")
    }
    return(assigned_subs)
}

#' Remove items from a data_list
#'
#' Removes specified elements from a provided data_list
#'
#' @param list_object The data_list containing components to be removed
#'
#' @param ... Any number of components to remove from the list object, passed as
#' strings
#'
#' @return A "list"-class object in which any specified elements have been
#' removed.
#'
#' @export
list_remove <- function(list_object, ...) {
    to_remove <- list(...)
    # Check to make sure all items to remove are components in list_object
    list_names <- summarize_dl(list_object)$"name"
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

#' Generate a complete path and filename to store an similarity matrix
#'
#' @param similarity_matrix_dir Directory to store similarity matrices
#' @param i Corresponding settings matrix row
#'
#' @return path Complete path and filename to store an similarity matrix
#'
#' @export
similarity_matrix_path <- function(similarity_matrix_dir, i) {
    path <- paste0(
        similarity_matrix_dir,
        "/",
        gsub("-", "_", Sys.Date()), # Today's datej
        "_",
        "similarity_matrix_",
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
#'
#' @param ... Remaining arguments for base::sample function
#'
#' @return Numeric vector result of running base::sample.
#'
#' @export
resample <- function(x, ...) {
    return(x[sample.int(length(x), ...)])
}

#' Check validity of similarity matrices
#'
#' Check to see if similarity matrices in a list have the following properties:
#'  1. The maximum value in the entire matrix is 0.5
#'  2. Every value in the diagonal is 0.5
#'
#' @param similarity_matrices A list of similarity matrices
#'
#' @return valid_matrices Boolean indicating if properties are met by all
#'  similarity matrices
#'
#' @export
check_similarity_matrices <- function(similarity_matrices) {
    valid_matrices <- similarity_matrices |>
        lapply(
            function(x) {
                max_along_diags <- diag(x) == max(x)
                max_diag_pt_5 <- diag(x) == 0.5
                return(max_along_diags & max_diag_pt_5)
            }
        ) |>
        unlist() |>
        all()
    if (!valid_matrices) {
        warning(
            "Generated similarity matrices did not meet validity parameters."
        )
    }
    return(valid_matrices)
}

#' Adjust the diagonals of a matrix
#'
#' Adjust the diagonals of a matrix to reduce contrast with off-diagonals
#' during plotting.
#'
#' @param matrix Matrix to rescale.
#'
#' @param method Method of rescaling. Can be:
#' * "mean" (replace diagonals with average value of off-diagonals)
#' * "zero" (replace diagonals with 0)
#' * "min" (replace diagonals with min value of off-diagonals)
#' * "max" (replace diagonals with max value of off-diagonals)
#'
#' @return A "matrix" class object with rescaled diagonals.
#'
#' @export
scale_diagonals <- function(matrix, method = "mean") {
    if (method == "mean") {
        off_diagonals <- matrix[col(matrix) != row(matrix)]
        diag(matrix) <- mean(off_diagonals)
    } else if (method == "zero") {
        diag(matrix) <- 0
    } else if (method == "min") {
        off_diagonals <- matrix[col(matrix) != row(matrix)]
        diag(matrix) <- min(off_diagonals)
    } else if (method == "max") {
        off_diagonals <- matrix[col(matrix) != row(matrix)]
        diag(matrix) <- max(off_diagonals)
    } else if (method != "none") {
        stop("Invalid scaling method specified.")
    }
    return(matrix)
}

#' Return a colour ramp for a given vector
#'
#' Given a numeric vector and min and max colour values, return a colour ramp
#' that assigns a colour to each element in the vector. This function is a
#' wrapper for `circlize::colorRamp2`.'
#'
#' @param data Vector of numeric values.
#'
#' @param min_colour Minimum colour value.
#'
#' @param max_colour Maximum colour value.
#'
#' @return A "function" class object that can build a circlize-style colour
#' ramp.
#'
#' @export
colour_scale <- function(data, min_colour, max_colour) {
    colours <- circlize::colorRamp2(
        c(min(data), max(data)),
        c(min_colour, max_colour)
    )
    return(colours)
}
