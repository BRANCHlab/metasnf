#' Developer constructor for data_list
#' 
#' @keywords internal
#' 
#' @param x A nested list to be converted into a `data_list` object.
#' 
#' @return A `data_list` object, which is a nested list with class `data_list`.
new_data_list <- function(x) {
    stopifnot(is.list(x))
    stopifnot(is.list(x[[1]]))
    dl <- structure(x, class = "data_list")
    return(dl)
}

#' Build a data list
#'
#' `data_list()` constructs a data list object with the classes 'data_list' and
#' `list`. This object is the primary way in which features to be used along
#' the metasnf clustering pipeline are stored. The data list is fundamentally
#' a 2-level nested list object where each inner list contains a data frame
#' (referenced as "data types" in the original similarity network fusion
#' article) and associated metadata for that data frame.
#' 
#' data list is a named and nested list containing input dataframes (data), the
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
#' @param train_uids character vector of train uids (useful if building
#' a full data list for label propagation)
#'
#' @param test_uids character vector of test uids (useful if building
#' a full data list for label propagation)
#'
#' @param sort_uids If TRUE, the uids in the data list will be sorted
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
#' dl <- generate_data_list(
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
#' dl <- generate_data_list(
#'     list(heart_rate_df, "heart_rate", "clinical", "continuous"),
#'     list(personality_test_df, "personality_test", "surveys", "continuous"),
#'     list(survey_response_df, "survey_response", "surveys", "ordinal"),
#'     list(city_df, "city", "location", "categorical"),
#'     uid = "patient_id"
#' )
#'
#' # Printing data list summaries
#' summarize_dl(dl)
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
data_list <- function(...,
                      uid = NULL,
                      test_uids = NULL,
                      train_uids = NULL,
                      sort_uids = TRUE) {
    # Initialize data list
    dl <- list()
    # Handle programmatic list-based loading
    loaded_data <- list(...)
    for (item in loaded_data) {
        standard_loaded <- methods::is(item[[1]], "data.frame")
        list_loaded <- methods::is(item[[1]], "list")
        if (standard_loaded) {
            dl <- append(dl, list(item))
        } else if (list_loaded) {
            dl <- append(dl, item)
        } else {
            stop(
                "Data must be formatted as either any number of lists each",
                " containing data (`data.frame`), name of data (`character`),",
                " domain of data (`character`), and type of data (`character`",
                " in c(\"continuous\", \"discrete\", \"ordinal\",",
                " \"categorical\", \"mixed\")) or as a single list of lists",
                " that each adhere to that format. See `?data_list` for more",
                " examples of valid inputs to this function."
            )
        }
    }
    # Assign names to the nested list elements
    named_entries <- dl |> lapply(
        function(x) {
            return(sum(names(x) == ""))
        }
    )
    if (all(named_entries == 0)) {
        dl_names <- c("data", "name", "domain", "type")
        dl <- lapply(dl, stats::setNames, dl_names)
    } else if (!(all(named_entries == 4))) {
        stop(
            "Please either specify names (i.e., data = ..., name = ...,",
            " domain = ..., type = ...) for all of the elements or for none",
            " of them."
        )
    }
    dl <- convert_uids(dl, uid)
    # Handle missing subject removal
    removal_results <- dl |> remove_dl_na(return_missing = TRUE)
    ###########################################################################
    dl <- dl |>
        reduce_dl_to_common() |>
        prefix_dl_sk()
    ###########################################################################
    # Sort uids alphabetically
    if (sort_uids) {
        dl <- dl |> arrange_dl()
    }
    ###########################################################################
    # Reposition the uid column to the first column in each dataframe
    dl <- dl_uid_first_col(dl)
    ###########################################################################
    # Ensure there are no duplicate feature names
    dl_has_duplicates(dl)
    ###########################################################################
    # Name the components of the data list
    names(dl) <- summarize_dl(dl)$"name"
    ###########################################################################
    # Class management
    validated <- validate_data_list(elements)
    dl <- new_data_list(validated)
    return(dl)
}

#' Convert unique identifiers of data list to "uid"
#'
#' Column name "uid" is reserved for the unique identifier of observations.
#' This function ensures all dataframes have their UID set as "uid".
#'
#' @param dl A nested list of input data from `generate_data_list()`.
#'
#' @param uid (string) the name of the uid column currently used data
#'
#' @return dl_renamed_id data list with "uid" as UID
#'
#' @export
convert_uids <- function(dl, uid = NULL) {
    # Column names of the first dataframe
    d1 <- dl[[1]]$"data"
    d1_cols  <- colnames(d1)
    # Check to see if uid is already present in the first dataframe
    if ("uid" %in% d1_cols) {
        # If uid exists and is a UID, leave the data list alone
        if (length(unique(d1$"uid")) == length(d1$"uid")) {
            return(dl)
        } else {
            # If uid exists and is not a UID, raise error
            stop(
                "Column `uid` exists, but it is not a unique ID.",
                " Please regenerate this data list after renaming",
                " the uid column or converting it to a UID column."
            )
        }
    }
    # This if only executes if uid doesn't exist as a column, but also
    #  there was no uid specified.
    if (is.null(uid)) {
        stop(paste0(
            "Please specify parameter 'uid' with the name of the column",
            " currently used as each row's unique identifier. This row will",
            " be converted to 'uid' for the remaining metasnf analyses."
        ))
    }
    # Check to ensure that the user specified UID exists in the data list
    if (!uid %in% d1_cols) {
        stop(paste0(
            "The specified original UID (", uid, ") is not present in",
            " this data list. Are you sure you spelled it correctly?"
        ))
    }
    # Convert the user specified original UID to 'uid'
    dl_renamed_id <- lapply(dl,
        function(x) {
            colnames(x$"data")[colnames(x$"data") == uid] <- "uid"
            x
        }
    )
    return(dl_renamed_id)
}

#' Remove NAs from a data list object
#'
#' @param dl A nested list of input data from `generate_data_list()`.
#'
#' @param return_missing If TRUE, function returns a list where the first
#'  element is the data list and the second element is a vector of unique IDs
#'  of patients who were removed during the complete data filtration step.
#'
#' @return dl A data list without NAs
#'
#' @export
remove_dl_na <- function(dl, return_missing = FALSE) {
    dl_no_nas <- lapply(
        dl,
        function(x) {
            x[[1]] <- stats::na.omit(x[[1]])
            return(x)
        }
    )
    # Return both the data list and missing patients based on return_missing
    if (return_missing) {
        all_data <- dl |> lapply(
            function(x) {
                x$"data"
            }
        ) |>
            merge_df_list(join = "full")
        all_uids <- all_data$"uid"
        complete_data <- stats::na.omit(all_data)
        complete_uids <- complete_data$"uid"
        complete_indices <- all_uids %in% complete_uids
        removed_uids <- all_uids[!complete_indices]
        results <- list(
            dl = dl_no_nas,
            removed_uids = removed_uids
        )
        return(results)
    } else {
        return(dl_no_nas)
    }
}

#' Add "uid_" prefix to all UID values in uid column
#'
#' @param dl A nested list of input data from `generate_data_list()`.
#'
#' @return dl A data list with UIDs prefixed with the string "uid_"
#'
#' @export
prefix_dl_sk <- function(dl) {
    dl_prefixed <- lapply(
        dl,
        function(x) {
            x[[1]]$"uid" <- paste0("uid_", x[[1]]$"uid")
            return(x)
        }
    )
    return(dl_prefixed)
}

#' Reduce data list to common uids
#'
#' Given a `data_list` object, reduce each nested dataframe to contain only the
#'  set of uids that are shared by all nested dataframes
#'
#' @param dl A nested list of input data from `generate_data_list()`.
#'
#' @return reduced_dl The data list object subsetted only to uidssnf
#'  shared across all nested dataframes
#' @export
reduce_dl_to_common <- function(dl) {
    uids <- lapply(dl, function(x) x[[1]]$"uid")
    data_objects <- lapply(dl, function(x) x[[1]])
    common_uids <- Reduce(intersect, uids)
    filtered_data_objects <- data_objects |>
        lapply(
            function(x) {
                dplyr::filter(x, x$"uid" %in% common_uids)
            }
        )
    reduced_dl <- dl
    for (i in seq_along(dl)) {
        reduced_dl[[i]][[1]] <- filtered_data_objects[[i]]
    }
    return(reduced_dl)
}

#' Given a data list object, sort data elements by uid
#'
#' @param dl A nested list of input data from `generate_data_list()`.
#'
#' @return arranged_dl The arranged data list object
#'
#' @export
arrange_dl <- function(dl) {
    data_objects <- lapply(dl, function(x) x[[1]])
    arranged_data_objects <- data_objects |>
        lapply(
            function(x) {
                dplyr::arrange(x, x$"uid")
            }
        )
    arranged_dl <- dl
    for (i in seq_along(dl)) {
        arranged_dl[[i]][[1]] <- arranged_data_objects[[i]]
    }
    return(arranged_dl)
}

#' Summarize a data list
#'
#' @param dl A nested list of input data from `generate_data_list()`.
#'
#' @param scope The level of detail for the summary. Options are:
#' - "component" (default): One row per component (dataframe) in the data list.
#' - "feature": One row for each feature in the data list.
#'
#' @return "data.frame"-class object summarizing all components (or features if
#' scope == "component").
#'
#' @export
summarize_dl <- function(dl, scope = "component") {
    if (scope == "component") {
        dl_summary <- data.frame(
            name = unlist(lapply(dl, function(x) x$"name")),
            type = unlist(lapply(dl, function(x) x$"type")),
            domain = unlist(domains(dl)),
            length = unlist(lapply(dl, function(x) dim(x$"data")[1])),
            width = unlist(lapply(dl, function(x) dim(x$"data")[2]))
        )
    } else if (scope == "feature") {
        dl_df <- collapse_dl(dl)
        dl_df <- dl_df[, colnames(dl_df) != "uid"]
        types <- dl |>
            lapply(
                function(x) {
                    rep(x$"type", ncol(x$"data") - 1)
                }
            ) |>
            unlist()
        domains <- dl |>
            lapply(
                function(x) {
                    rep(x$"domain", ncol(x$"data") - 1)
                }
            ) |>
            unlist()
        var_names <- colnames(dl_df[, colnames(dl_df) != "uid"])
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
#' @param dl A nested list of input data from `generate_data_list()`.
#'
#' @return domain_list list of domains
#'
#' @export
domains <- function(dl) {
    domain_list <- lapply(dl, function(x) x$"domain")
    return(domain_list)
}

#' Collapse a dl into a single dataframe
#'
#' @param dl A nested list of input data from `generate_data_list()`.
#'
#' @return A "data.frame"-formatted version of the provided data list.
#'
#' @export
collapse_dl <- function(dl) {
    data_only <- dl |> lapply(
        function(x) {
            return(x$"data")
        }
    )
    merged_df <- merge_df_list(data_only)
    return(merged_df)
}

#' Variable-level summary of a data list
#'
#' @param dl A nested list of input data from `generate_data_list()`.
#'
#' @return variable_level_summary A dataframe containing the name, type, and
#' domain of every variable in a data list.
#'
#' @export
dl_variable_summary <- function(dl) {
    types <- dl |>
        lapply(
            function(x) {
                rep(x$"type", ncol(x$"data") - 1)
            }
        ) |>
        unlist()
    domains <- dl |>
        lapply(
            function(x) {
                rep(x$"domain", ncol(x$"data") - 1)
            }
        ) |>
        unlist()
    merged_df <- dl |>
        collapse_dl() |>
        data.frame()
    var_names <- colnames(merged_df[, colnames(merged_df) != "uid"])
    variable_level_summary <- data.frame(
        name = var_names,
        type = types,
        domain = domains
    )
    return(variable_level_summary)
}

#' Reorder the uids in a data list
#'
#' @param dl A nested list of input data from `generate_data_list()`.
#'
#' @param ordered_uids A vector of the uid values in the data list
#' in the desired order of the sorted data list.
#'
#' @return A data list ("list"-class object) with reordered observations.
#'
#' @export
reorder_dl_subs <- function(dl, ordered_uids) {
    dl <- dl |>
        lapply(
            function(x) {
                index <- match(x$"data"$"uid", ordered_uids)
                x$"data" <- x$"data"[order(index), ]
                return(x)
            }
        )
    return(dl)
}

#' Rename features in a data list
#'
#' @param dl A nested list of input data from `generate_data_list()`.
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
#' dl <- generate_data_list(
#'     list(pubertal, "pubertal_status", "demographics", "continuous"),
#'     list(anxiety, "anxiety", "behaviour", "ordinal"),
#'     list(depress, "depressed", "behaviour", "ordinal"),
#'     uid = "unique_id"
#' )
#'
#' summarize_dl(dl, "feature")
#'
#' name_changes <- c(
#'     "anxiety_score" = "cbcl_anxiety_r",
#'     "depression_score" = "cbcl_depress_r"
#' )
#'
#' dl <- rename_dl(dl, name_changes)
#'
#' summarize_dl(dl, "feature")
rename_dl <- function(dl, name_mapping) {
    dl_features <- summarize_dl(dl, "feature")$"name"
    mismatches <- which(!name_mapping %in% dl_features)
    if (length(mismatches) > 0) {
        warning(
            "The following feature names were not found in the provided",
            " dl: ", name_mapping[mismatches]
        )
    }
    dl <- dl |> lapply(
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
    return(dl)
}

#' Extract uids from a data list
#'
#' @param dl A nested list of input data from `generate_data_list()`.
#'
#' @param prefix If TRUE, preserves the "uid_" prefix added to UIDs when
#' creating a data list.
#'
#' @return A character vector of the UID labels contained in a data list.
#'
#' @export
get_dl_uids <- function(dl, prefix = FALSE) {
    dl_df <- collapse_dl(dl)
    uids <- dl_df$"uid"
    if (prefix) {
        return(uids)
    }
    uids <- gsub("uid_", "", uids)
    return(uids)
}

#' Make the uid UID columns of a data list first
#'
#' @param dl A nested list of input data from `generate_data_list()`.
#'
#' @return A data list ("list"-class object) in which each data-subcomponent
#' has "uid" positioned as its first column.
#'
#' @export
dl_uid_first_col <- function(dl) {
    dl <- lapply(
        dl,
        function(x) {
            x$"data" <- x$"data" |>
                dplyr::select(
                    "uid",
                    dplyr::everything()
                )
            return(x)
        }
    )
}

#' Horizontally merge compatible data lists
#'
#' Join two data lists with the same components (dataframes) but separate
#' observations. To instead merge two data lists that have the same
#' observations but different components, simply use `c()`.
#'
#' @param dl_1 The first data list to merge.
#'
#' @param dl_2 The second data list to merge.
#'
#' @return A data list ("list"-class object) containing the observations of
#' both provided data lists.
#'
#' @export
merge_dls <- function(dl_1, dl_2) {
    dl_1_names <- summarize_dl(dl_1)$"name"
    dl_2_names <- summarize_dl(dl_2)$"name"
    names(dl_1) <- dl_1_names
    names(dl_2) <- dl_2_names
    if (!identical(sort(dl_1_names), sort(dl_2_names))) {
        stop(
            "The two data lists must have identical components. Check",
            " `summarize_dl()` on each data list to make sure the components",
            " align."
        )
    }
    merged_data_list <- lapply(
        dl_1_names,
        function(x) {
            dl_1[[x]]$"data" <- dplyr::bind_rows(
                dl_1[[x]]$"data",
                dl_2[[x]]$"data"
            )
            return(dl_1[[x]])
        }
    )
    names(merged_data_list) <- dl_1_names
    return(merged_data_list)
}

#' Check if data list contains any duplicate features
#'
#' @param dl A nested list of input data from `generate_data_list()`.
#'
#' @return Doesn't return any value. Raises warning if there are features
#' with duplicate names in a generated data list.
#'
#' @export
dl_has_duplicates <- function(dl) {
    features <- dl |> lapply(
        function(x) {
            return(colnames(x$"data")[-1])
        }
    ) |>
        unlist() |>
        as.character()
    duplicates <- unique(features[duplicated(features)])
    if (length(duplicates) > 0) {
        warning(
            "Generated data list has duplicate feature names, which can",
            " cause problems with downstream analyses."
        )
    }
}
