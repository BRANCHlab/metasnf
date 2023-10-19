#' Generate data_list object - but softcoded
#'
#' This is the major data object that will be processed when iterating through
#'  the settings matrix. The full list contains one list per measurement type.
#'  Within each measurement type's list, elements include the actual data
#'  structure, the name, the domain, and the data 'type' (i.e, numeric or
#'  categorical).
#'
#' To-do: include checks to make sure format of data list is correct
#'
#' @param ... Any number of list formatted as (df, "df_name", "df_domain",
#'  "df_type") OR any number of lists of lists formatted as (df, "df_name",
#'  "df_domain", "df_type")
#' @param uid (string) the name of the uid column currently used data
#' @param train_subjects character vector of train subjects (useful if building
#'  a full data list for label propagation)
#' @param test_subjects character vector of test subjects (useful if building
#'  a full data list for label propagation)
#' @param assigned_splits ouptut from assign_splits function - can be given
#'  as an alternative to specifying the train/test subjects separately.
#'
#' @export
#' @examples
#' heart_rate_df <- data.frame(
#'     patient_id = c("1", "2", "3"),
#'     var1 = c(0.04, 0.1, 0.3),
#'     var2 = c(30, 2, 0.3)
#' )
#'
#' personality_test_df <- data.frame(
#'     patient_id = c("1", "2", "3"),
#'     var3 = c(900, 1990, 373),
#'     var4 = c(509, 2209, 83)
#' )
#'
#' dl <- generate_data_list(
#'     list(heart_rate_df, "data1", "domain1", "numeric"),
#'     list(personality_test_df, "data2", "domain2", "numeric"),
#'     uid = "patient_id"
#' )
#'
#' # Alternative loading: providing a single list of lists
#'
#' list_of_lists <- list(
#'     list(heart_rate_df, "data1", "domain1", "numeric"),
#'     list(personality_test_df, "data2", "domain2", "numeric")
#' )
#'
#' dl <- generate_data_list(
#'     list_of_lists,
#'     uid = "patient_id"
#' )
generate_data_list <- function(..., uid = NULL, test_subjects = NULL,
                               train_subjects = NULL, assigned_splits = NULL) {
    subjectkey <- "" # trickery to avoid build errors - fix this later
    # The object that will contain all the data
    data_list <- list()
    # The loaded data
    loaded_data <- list(...)
    for (item in loaded_data) {
        if (methods::is(item[[1]], "data.frame")) {
            # A standard loaded data item (a 4-component list)
            data_list <- append(data_list, list(item))
        } else if (methods::is(item[[1]], "list")) {
            # A bulk loaded data item (list of 4-component lists)
            data_list <- append(data_list, item)
        }
    }
    # To-do: add tests to catch invalid inputs in this function.
    # Assign names to the nested list elements
    data_list_names <- c("data", "name", "domain", "type")
    data_list <- lapply(data_list, stats::setNames, data_list_names)
    data_list <- convert_uids(data_list, uid)
    data_list <- data_list |>
        remove_dl_na() |>
        reduce_dl_to_common() |>
        prefix_dl_sk()
    # Correctly order train and test subjects for label prop
    if (!is.null(test_subjects) & !is.null(train_subjects)) {
        # If test subjects and train subjects are provided, arrange dl subs
        #  to follow the order of train subjects followed by test subjects
        tts <- paste0("subject_", c(train_subjects, test_subjects))
        data_list <- data_list |>
            lapply(
                function(x) {
                    x$"data" <- x$"data" |> dplyr::arrange(
                        sapply(
                            subjectkey,
                            function(x) which(x == tts)
                        )
                    )
                    return(x)
                }
            )
    } else if (!is.null(assigned_splits)) {
        # An alternative input to providing test and train subjects
        train_subjects <- assigned_splits |>
            dplyr::filter(split == "train")
        train_subjects <- train_subjects$"subjectkey"
        test_subjects <- assigned_splits |>
            dplyr::filter(split == "test")
        test_subjects <- test_subjects$"subjectkey"
        tts <- paste0("subject_", c(train_subjects, test_subjects))
        data_list <- data_list |>
            lapply(
                function(x) {
                    x$"data" <- x$"data" |> dplyr::arrange(
                        sapply(
                            subjectkey,
                            function(x) which(x == tts)
                        )
                    )
                    return(x)
                }
            )
    } else {
        # If no order is specified, just sort the subjects alphabetically
        data_list <- data_list |> arrange_dl()
    }
    return(data_list)
}

#' Convert unique identifiers of data_list to 'subjectkey'
#'
#' Column name "subjectkey" is reserved for the unique identifier of subjects.
#'  This function ensures all dataframes have their UID set as "subjectkey".
#'
#' @param data_list a data_list
#' @param uid (string) the name of the uid column currently used data
#'
#' @return dl_renamed_id data list with 'subjectkey' as UID
#'
#' @export
convert_uids <- function(data_list, uid = NULL) {
    # Column names of the first dataframe
    d1 <- data_list[[1]]$"data"
    d1_cols  <- colnames(d1)
    # Check to see if subjectkey is already present in the first dataframe
    if ("subjectkey" %in% d1_cols) {
        # If subjectkey exists and is a UID, leave the data_list alone
        if (length(unique(d1$"subjectkey")) == length(d1$"subjectkey")) {
            print("Existing `subjectkey` column will be treated as UID.")
            return(data_list)
        # If subjectkey exists and is not a UID, raise error
        } else {
            stop(paste0(
                "Column `subjectkey` exists, but it is not a unique ID.",
                " Please regenerate this data_list after renaming",
                " the subjectkey column or converting it to a UID column."
            ))
        }
    }
    # This if only executes if subjectkey doesn't exist as a column, but also
    #  there was no uid specified.
    if (is.null(uid)) {
        stop(paste0(
            "Please specify parameter 'uid' with the name of the column",
            " currently used as each row's unique identifier. This row will",
            " be converted to 'subjectkey' for the remaining metasnf analyses."
        ))
    }
    # Check to ensure that the user specified UID exists in the data_list
    if (!uid %in% d1_cols) {
        stop(paste0(
            "The specified original UID (", uid, ") is not present in",
            " this data list. Are you sure you spelled it correctly?"
        ))
    }
    # Convert the user specified original UID to 'subjectkey'
    dl_renamed_id <- lapply(data_list,
        function(x) {
            colnames(x$"data")[colnames(x$"data") == uid] <- "subjectkey"
            x
        }
    )
    print("UID successfully converted to subjectkey.")
    return(dl_renamed_id)
}

#' Remove NAs from a data_list object
#'
#' @param data_list A data_list
#'
#' @return data_list A data_list without NAs
#'
#' @export
remove_dl_na <- function(data_list) {
    dl_no_nas <- lapply(
        data_list,
        function(x) {
            x[[1]] <- stats::na.omit(x[[1]])
            return(x)
        }
    )
    return(dl_no_nas)
}

#' Add "subject_" prefix to all UID values in subjectkey column
#'
#' @param data_list A data_list
#'
#' @return data_list A data_list without NAs
#'
#' @export
prefix_dl_sk <- function(data_list) {
    dl_prefixed <- lapply(
        data_list,
        function(x) {
            x[[1]]$"subjectkey" <- paste0("subject_", x[[1]]$"subjectkey" )
            return(x)
        }
    )
    return(dl_prefixed)
}

#' Reduce data_list to common subjects
#'
#' Given a `data_list` object, reduce each nested dataframe to contain only the
#'  set of subjects that are shared by all nested dataframes
#'
#' @param data_list The data_list object to be reduced
#'
#' @return reduced_data_list The data_list object subsetted only to subjectssnf
#'  shared across all nested dataframes
#' @export
reduce_dl_to_common <- function(data_list) {
    subjects <- lapply(data_list, function(x) x[[1]]$"subjectkey")
    data_objects <- lapply(data_list, function(x) x[[1]])
    common_subjects <- Reduce(intersect, subjects)
    filtered_data_objects <-
        lapply(data_objects,
        function(x) {
            dplyr::filter(x, x$"subjectkey" %in% common_subjects)
        })
    reduced_data_list <- data_list
    for (i in seq_along(data_list)) {
        reduced_data_list[[i]][[1]] <- filtered_data_objects[[i]]
    }
    return(reduced_data_list)
}

#' Given a data_list object, sort data elements by subjectkey
#'
#' @param data_list The data_list object to be arranged
#'
#' @return arranged_data_list The arranged data_list object
#'
#' @export
arrange_dl <- function(data_list) {
    data_objects <- lapply(data_list, function(x) x[[1]])
    arranged_data_objects <-
        lapply(data_objects,
        function(x) {
            dplyr::arrange(x, x$"subjectkey")
        })
    arranged_data_list <- data_list
    for (i in seq_along(data_list)) {
        arranged_data_list[[i]][[1]] <- arranged_data_objects[[i]]
    }
    return(arranged_data_list)
}

#' Summarize data list
#'
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#'
#' @return dl_summary Summarized output
#'
#' @export
summarize_dl <- function(data_list) {
    dl_summary <-
        data.frame(
            name = unlist(lapply(data_list, function(x) x$"name")),
            type = unlist(lapply(data_list, function(x) x$"type")),
            domain = unlist(domains(data_list)),
            length = unlist(lapply(data_list, function(x) dim(x$"data")[1])),
            width = unlist(lapply(data_list, function(x) dim(x$"data")[2])))
    return(dl_summary)
}

#' Domains
#'
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#'
#' @return domain_list list of domains
#'
#' @export
domains <- function(data_list) {
    domain_list <- lapply(data_list, function(x) x$"domain")
    return(domain_list)
}
