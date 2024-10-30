#' Generate a data_list
#'
#' This function generates the major data object that will be processed when
#' iterating through the each SNF pipeline defined in the settings_matrix. The
#' data_list is a named and nested list containing input dataframes (data), the
#' name of that input dataframe (for the user's reference), the 'domain' of
#' that dataframe (the broader source of information that the input dataframe
#' is capturing, determined by user's domain knowledge), and the type of
#' feature stored in the dataframe (continuous, discrete, ordinal,
#' categorical, or mixed).
#'
#' @param ... Any number of list formatted as (df, "df_name", "df_domain",
#' "df_type") OR any number of lists of lists formatted as (df, "df_name",
#' "df_domain", "df_type")
#'
#' @param uid (string) the name of the uid column currently used data
#'
#' @param train_subjects character vector of train subjects (useful if building
#' a full data list for label propagation)
#'
#' @param test_subjects character vector of test subjects (useful if building
#' a full data list for label propagation)
#'
#' @param sort_subjects If TRUE, the subjects in the data_list will be sorted
#'
#' @param remove_missing If TRUE (default), subjects with incomplete data will
#' be dropped from data_list creation. Setting this value to FALSE may lead to
#' unusual and/or unstable results during SNF, clustering, p-value calculations
#' or label propagation.
#'
#' @param return_missing If TRUE, function returns a list where the first
#' element is the data_list and the second element is a vector of unique IDs
#' of patients who were removed during the complete data filtration step.
#'
#' @return A nested "list" class object. Each list component contains a 4-item
#' list of a data frame, the user-assigned name of the data frame, the
#' user-assigned domain of the data frame, and the user-labeled type of the
#' data frame.
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
#' survey_response_df <- data.frame(
#'     patient_id = c("1", "2", "3"),
#'     var5 = c(1, 3, 3),
#'     var6 = c(2, 3, 3)
#' )
#'
#' city_df <- data.frame(
#'     patient_id = c("1", "2", "3"),
#'     var7 = c("toronto", "montreal", "vancouver")
#' )
#'
#' # Explicitly (Name each nested list element):
#' data_list <- generate_data_list(
#'     list(
#'         data = heart_rate_df,
#'         name = "heart_rate",
#'         domain = "clinical",
#'         type = "continuous"
#'     ),
#'     list(
#'         data = personality_test_df,
#'         name = "personality_test",
#'         domain = "surveys",
#'         type = "continuous"
#'     ),
#'     list(
#'         data = survey_response_df,
#'         name = "survey_response",
#'         domain = "surveys",
#'         type = "ordinal"
#'     ),
#'     list(
#'         data = city_df,
#'         name = "city",
#'         domain = "location",
#'         type = "categorical"
#'     ),
#'     uid = "patient_id"
#' )
#'
#' # Compact loading
#' data_list <- generate_data_list(
#'     list(heart_rate_df, "heart_rate", "clinical", "continuous"),
#'     list(personality_test_df, "personality_test", "surveys", "continuous"),
#'     list(survey_response_df, "survey_response", "surveys", "ordinal"),
#'     list(city_df, "city", "location", "categorical"),
#'     uid = "patient_id"
#' )
#'
#' # Printing data_list summaries
#' summarize_dl(data_list)
#'
#' # Alternative loading: providing a single list of lists
#' list_of_lists <- list(
#'     list(heart_rate_df, "data1", "domain1", "continuous"),
#'     list(personality_test_df, "data2", "domain2", "continuous")
#' )
#'
#' dl <- generate_data_list(
#'     list_of_lists,
#'     uid = "patient_id"
#' )
generate_data_list <- function(...,
                               uid = NULL,
                               test_subjects = NULL,
                               train_subjects = NULL,
                               sort_subjects = TRUE,
                               remove_missing = TRUE,
                               return_missing = FALSE) {
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
    # Assign names to the nested list elements
    named_entries <- data_list |> lapply(
        function(x) {
            return(sum(names(x) == ""))
        }
    )
    if (all(named_entries == 0)) {
        data_list_names <- c("data", "name", "domain", "type")
        data_list <- lapply(data_list, stats::setNames, data_list_names)
    } else if (!(all(named_entries == 4))) {
        stop(
            "Please either specify names (i.e., data = ..., name = ...,",
            " domain = ..., type = ...) for all of the elements or for none",
            " of them."
        )
    }
    data_list <- convert_uids(data_list, uid)
    ###########################################################################
    # Handle missing subject removal
    ###########################################################################
    removal_results <- data_list |> remove_dl_na(return_missing = TRUE)
    if (remove_missing) {
        n_dropped <- length(removal_results$"removed_subjects")
        if (n_dropped > 0) {
            warning(n_dropped, " subject(s) dropped due to incomplete data.")
        }
        data_list <- removal_results$"data_list"
    }
    ###########################################################################
    data_list <- data_list |>
        reduce_dl_to_common() |>
        prefix_dl_sk()
    ###########################################################################
    # Sort subjects alphabetically
    if (sort_subjects) {
        data_list <- data_list |> arrange_dl()
    }
    ###########################################################################
    # Reposition the subjectkey column to the first column in each dataframe
    ###########################################################################
    data_list <- dl_uid_first_col(data_list)
    ###########################################################################
    # Ensure there are no duplicate feature names
    ###########################################################################
    dl_has_duplicates(data_list)
    ###########################################################################
    # Name the components of the data list
    ###########################################################################
    names(data_list) <- summarize_dl(data_list)$"name"
    ###########################################################################
    # Return output
    ###########################################################################
    if (return_missing) {
        removed_subjects <- removal_results$"removed_subjects"
        results <- list(
            data_list = data_list,
            removed_subjects = removed_subjects
        )
        return(results)
    } else {
        return(data_list)
    }
}

#' Convert unique identifiers of data_list to 'subjectkey'
#'
#' Column name "subjectkey" is reserved for the unique identifier of subjects.
#'  This function ensures all dataframes have their UID set as "subjectkey".
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
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
            message("Existing `subjectkey` column will be treated as UID.")
            return(data_list)
        } else {
            # If subjectkey exists and is not a UID, raise error
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
    return(dl_renamed_id)
}

#' Remove NAs from a data_list object
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @param return_missing If TRUE, function returns a list where the first
#'  element is the data_list and the second element is a vector of unique IDs
#'  of patients who were removed during the complete data filtration step.
#'
#' @return data_list A data_list without NAs
#'
#' @export
remove_dl_na <- function(data_list, return_missing = FALSE) {
    dl_no_nas <- lapply(
        data_list,
        function(x) {
            x[[1]] <- stats::na.omit(x[[1]])
            return(x)
        }
    )
    # Return both the data list and missing patients based on return_missing
    if (return_missing) {
        all_data <- data_list |> lapply(
            function(x) {
                x$"data"
            }
        ) |>
            merge_df_list(join = "full")
        all_subjects <- all_data$"subjectkey"
        complete_data <- stats::na.omit(all_data)
        complete_subjects <- complete_data$"subjectkey"
        complete_indices <- all_subjects %in% complete_subjects
        removed_subjects <- all_subjects[!complete_indices]
        results <- list(
            data_list = dl_no_nas,
            removed_subjects = removed_subjects
        )
        return(results)
    } else {
        return(dl_no_nas)
    }
}

#' Add "subject_" prefix to all UID values in subjectkey column
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @return data_list A data_list without NAs
#'
#' @export
prefix_dl_sk <- function(data_list) {
    dl_prefixed <- lapply(
        data_list,
        function(x) {
            x[[1]]$"subjectkey" <- paste0("subject_", x[[1]]$"subjectkey")
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
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @return reduced_data_list The data_list object subsetted only to subjectssnf
#'  shared across all nested dataframes
#' @export
reduce_dl_to_common <- function(data_list) {
    subjects <- lapply(data_list, function(x) x[[1]]$"subjectkey")
    data_objects <- lapply(data_list, function(x) x[[1]])
    common_subjects <- Reduce(intersect, subjects)
    filtered_data_objects <- data_objects |>
        lapply(
            function(x) {
                dplyr::filter(x, x$"subjectkey" %in% common_subjects)
            }
        )
    reduced_data_list <- data_list
    for (i in seq_along(data_list)) {
        reduced_data_list[[i]][[1]] <- filtered_data_objects[[i]]
    }
    return(reduced_data_list)
}

#' Given a data_list object, sort data elements by subjectkey
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @return arranged_data_list The arranged data_list object
#'
#' @export
arrange_dl <- function(data_list) {
    data_objects <- lapply(data_list, function(x) x[[1]])
    arranged_data_objects <- data_objects |>
        lapply(
            function(x) {
                dplyr::arrange(x, x$"subjectkey")
            }
        )
    arranged_data_list <- data_list
    for (i in seq_along(data_list)) {
        arranged_data_list[[i]][[1]] <- arranged_data_objects[[i]]
    }
    return(arranged_data_list)
}

#' Summarize a data list
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @param scope The level of detail for the summary. Options are:
#' - "component" (default): One row per component (dataframe) in the data_list.
#' - "feature": One row for each feature in the data_list.
#'
#' @return "data.frame"-class object summarizing all components (or features if
#' scope == "component").
#'
#' @export
summarize_dl <- function(data_list, scope = "component") {
    if (scope == "component") {
        dl_summary <- data.frame(
            name = unlist(lapply(data_list, function(x) x$"name")),
            type = unlist(lapply(data_list, function(x) x$"type")),
            domain = unlist(domains(data_list)),
            length = unlist(lapply(data_list, function(x) dim(x$"data")[1])),
            width = unlist(lapply(data_list, function(x) dim(x$"data")[2]))
        )
    } else if (scope == "feature") {
        dl_df <- collapse_dl(data_list)
        dl_df <- dl_df[, colnames(dl_df) != "subjectkey"]
        types <- data_list |>
            lapply(
                function(x) {
                    rep(x$"type", ncol(x$"data") - 1)
                }
            ) |>
            unlist()
        domains <- data_list |>
            lapply(
                function(x) {
                    rep(x$"domain", ncol(x$"data") - 1)
                }
            ) |>
            unlist()
        var_names <- colnames(dl_df[, colnames(dl_df) != "subjectkey"])
        dl_summary <- data.frame(
            name = var_names,
            type = types,
            domain = domains
        )
    }
    rownames(dl_summary) <- seq_len(nrow(dl_summary))
    return(dl_summary)
}

#' Domains
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @return domain_list list of domains
#'
#' @export
domains <- function(data_list) {
    domain_list <- lapply(data_list, function(x) x$"domain")
    return(domain_list)
}

#' Collapse a data_list into a single dataframe
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @return A "data.frame"-formatted version of the provided data list.
#'
#' @export
collapse_dl <- function(data_list) {
    data_only <- data_list |> lapply(
        function(x) {
            return(x$"data")
        }
    )
    merged_df <- merge_df_list(data_only)
    return(merged_df)
}

#' Variable-level summary of a data_list
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @return variable_level_summary A dataframe containing the name, type, and
#' domain of every variable in a data_list.
#'
#' @export
dl_variable_summary <- function(data_list) {
    types <- data_list |>
        lapply(
            function(x) {
                rep(x$"type", ncol(x$"data") - 1)
            }
        ) |>
        unlist()
    domains <- data_list |>
        lapply(
            function(x) {
                rep(x$"domain", ncol(x$"data") - 1)
            }
        ) |>
        unlist()
    merged_df <- data_list |>
        collapse_dl() |>
        data.frame()
    var_names <- colnames(merged_df[, colnames(merged_df) != "subjectkey"])
    variable_level_summary <- data.frame(
        name = var_names,
        type = types,
        domain = domains
    )
    return(variable_level_summary)
}

#' Reorder the subjects in a data_list
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @param ordered_subjects A vector of the subjectkey values in the data_list
#' in the desired order of the sorted data_list.
#'
#' @return A data list ("list"-class object) with reordered observations.
#'
#' @export
reorder_dl_subs <- function(data_list, ordered_subjects) {
    data_list <- data_list |>
        lapply(
            function(x) {
                index <- match(x$"data"$"subjectkey", ordered_subjects)
                x$"data" <- x$"data"[order(index), ]
                return(x)
            }
        )
    return(data_list)
}

#' Rename features in a data_list
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @param name_mapping A named vector where the values are the features to be
#' renamed and the names are the new names for those features.
#'
#' @return A data list ("list"-class object) with adjusted feature names.
#'
#' @export
#' @examples
#'
#' library(metasnf)
#'
#' data_list <- generate_data_list(
#'     list(pubertal, "pubertal_status", "demographics", "continuous"),
#'     list(anxiety, "anxiety", "behaviour", "ordinal"),
#'     list(depress, "depressed", "behaviour", "ordinal"),
#'     uid = "unique_id"
#' )
#'
#' summarize_dl(data_list, "feature")
#'
#' name_changes <- c(
#'     "anxiety_score" = "cbcl_anxiety_r",
#'     "depression_score" = "cbcl_depress_r"
#' )
#'
#' data_list <- rename_dl(data_list, name_changes)
#'
#' summarize_dl(data_list, "feature")
rename_dl <- function(data_list, name_mapping) {
    dl_features <- summarize_dl(data_list, "feature")$"name"
    mismatches <- which(!name_mapping %in% dl_features)
    if (length(mismatches) > 0) {
        warning(
            "The following feature names were not found in the provided",
            " data_list: ", name_mapping[mismatches]
        )
    }
    data_list <- data_list |> lapply(
        function(x) {
            old_colnames <- colnames(x$"data")
            new_colnames <- old_colnames |> lapply(
                function(old_name) {
                    if (old_name %in% name_mapping) {
                        name_match <- which(name_mapping == old_name)
                        new_name <- names(name_mapping)[name_match]
                    } else {
                        new_name <- old_name
                    }
                    return(new_name)
                }
            )
            colnames(x$"data") <- new_colnames
            return(x)
        }
    )
    return(data_list)
}

#' Extract subjects from a data_list
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @param prefix If TRUE, preserves the "subject_" prefix added to UIDs when
#' creating a data_list.
#'
#' @return A character vector of the UID labels contained in a data list.
#'
#' @export
get_dl_subjects <- function(data_list, prefix = FALSE) {
    dl_df <- collapse_dl(data_list)
    subjects <- dl_df$"subjectkey"
    if (prefix) {
        return(subjects)
    }
    subjects <- gsub("subject_", "", subjects)
    return(subjects)
}

#' Make the subjectkey UID columns of a data_list first
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @return A data list ("list"-class object) in which each data-subcomponent
#' has "subjectkey" positioned as its first column.
#'
#' @export
dl_uid_first_col <- function(data_list) {
    data_list <- lapply(
        data_list,
        function(x) {
            x$"data" <- x$"data" |>
                dplyr::select(
                    "subjectkey",
                    dplyr::everything()
                )
            return(x)
        }
    )
}

#' Horizontally merge compatible data lists
#'
#' Join two data_lists with the same components (dataframes) but separate
#' observations. To instead merge two data_lists that have the same
#' observations but different components, simply use `c()`.
#'
#' @param data_list1 The first data_list to merge.
#'
#' @param data_list2 The second data_list to merge.
#'
#' @return A data list ("list"-class object) containing the observations of
#' both provided data lists.
#'
#' @export
merge_data_lists <- function(data_list1, data_list2) {
    dl1_names <- summarize_dl(data_list1)$"name"
    dl2_names <- summarize_dl(data_list2)$"name"
    names(data_list1) <- dl1_names
    names(data_list2) <- dl2_names
    if (!identical(sort(dl1_names), sort(dl2_names))) {
        stop(
            "The two data lists must have identical components. Check",
            " `summarize_dl()` on each data list to make sure the components",
            " align."
        )
    }
    merged_data_list <- lapply(
        dl1_names,
        function(x) {
            data_list1[[x]]$"data" <- dplyr::bind_rows(
                data_list1[[x]]$"data",
                data_list2[[x]]$"data"
            )
            return(data_list1[[x]])
        }
    )
    names(merged_data_list) <- dl1_names
    return(merged_data_list)
}

#' Check if data list contains any duplicate features
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @return Doesn't return any value. Raises warning if there are features
#' with duplicate names in a generated data list.
#'
#' @export
dl_has_duplicates <- function(data_list) {
    features <- data_list |> lapply(
        function(x) {
            return(colnames(x$"data")[-1])
        }
    ) |>
        unlist() |>
        as.character()
    duplicates <- unique(features[duplicated(features)])
    if (length(duplicates) > 0) {
        warning(
            "Generated data_list has duplicate feature names, which can",
            " cause problems with downstream analyses."
        )
    }
}
