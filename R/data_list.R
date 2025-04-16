#' Build a `data_list` class object
#'
#' `data_list()` constructs a data list object which inherits from classes
#' `data_list` and `list`. This object is the primary way in which features to
#' be used along the `metasnf` clustering pipeline are stored. The data list is
#' fundamentally a 2-level nested list object where each inner list contains a
#' data frame and associated metadata for that data frame. The metadata
#' includes the name of the data frame, the 'domain' of that data frame (the
#' broader source of information that the input data frame is capturing,
#' determined by user's domain knowledge), and the type of feature stored in
#' the data frame (continuous, discrete, ordinal, categorical, or mixed).
#'
#' @param ... Any number of lists formatted as (df, "df_name", "df_domain",
#'  "df_type") and/or any number of lists of lists formatted as (df, "df_name",
#'  "df_domain", "df_type").
#' @param uid (character) the name of the uid column currently used data.
#'  data frame.
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
#' dl <- data_list(
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
#' dl <- data_list(
#'     list(heart_rate_df, "heart_rate", "clinical", "continuous"),
#'     list(personality_test_df, "personality_test", "surveys", "continuous"),
#'     list(survey_response_df, "survey_response", "surveys", "ordinal"),
#'     list(city_df, "city", "location", "categorical"),
#'     uid = "patient_id"
#' )
#'
#' # Printing data list summaries
#' summary(dl)
#'
#' # Alternative loading: providing a single list of lists
#' list_of_lists <- list(
#'     list(heart_rate_df, "data1", "domain1", "continuous"),
#'     list(personality_test_df, "data2", "domain2", "continuous")
#' )
#'
#' dl <- data_list(
#'     list_of_lists,
#'     uid = "patient_id"
#' )
data_list <- function(...,
                      uid) {
    # Initialize data list-like list object
    dll <- list()
    # Handle programmatic list-based loading
    loaded_data <- list(...)
    check_dll_empty_input(loaded_data)
    for (item in loaded_data) {
        standard_loaded <- methods::is(item[[1]], "data.frame")
        list_loaded <- methods::is(item[[1]], "list")
        if (standard_loaded) {
            dll <- append(dll, list(item))
        } else if (list_loaded) {
            dll <- append(dll, item)
        } else {
            metasnf_error(
                "Invalid data loading format. See `?data_list` for examples o",
                "n proper data formatting."
            )
        }
    }
    # Assign names to the nested list elements
    all_correct_names <- dll |>
        lapply(
            function(x) {
                all(c("data", "name", "domain", "type") %in% names(x))
            }
        ) |>
        unlist() |>
        all()
    all_null_names <- dll |> lapply(
        function(x) {
            is.null(names(x))
        }
    ) |>
        unlist() |>
        all()
    if (all_null_names) {
        dll_names <- c("data", "name", "domain", "type")
        dll <- lapply(dll, stats::setNames, dll_names)
    } else if (all_correct_names) {
        dll <- lapply(
            dll,
            function(x) {
                x[c("data", "name", "domain", "type")]
            }
        )
    } else {
        metasnf_error(
            "Please either specify names (i.e., data = ...,",
            " name = ..., domain = ..., type = ...) for all of the",
            " elements or for none of them."
        )
    }
    # Additional formatting
    dll <- dll |>
        ensure_dll_df() |> # format the "data" item as a data frame
        convert_uids(uid) |> # Convert data frame UID column to "uid"
        remove_dll_incomplete() |> # drop observations without complete data
        prefix_dll_uid() |> # append "uid_" to the literal UID values
        arrange_dll() |> # sort observations in contained data frames by UID
        dll_uid_first_col() # position "uid" column at start of each data frame
    # Name the components of the data list
    names(dll) <- sapply(dll, function(x) x$"name")
    # Class management
    dll <- validate_data_list(dll)
    dl <- new_data_list(dll)
    return(dl)
}

#' Ensure the data item of each component is a `data.frame` class object
#'
#' @keywords internal
#' @param dll A data list-like `list` class object.
#' @return The provided dll with the data item of each component as a
#'  data frame.
ensure_dll_df <- function(dll) {
    lapply(
        dll,
        function(x) {
            x$"data" <- data.frame(x$"data")
            return(x)
        }
    )
}

#' Convert unique identifiers of data list to "uid"
#'
#' Column name "uid" is reserved for the unique identifier of observations.
#' This function ensures all data frames have their UID set as "uid".
#'
#' @keywords internal
#' @param dll A data list-like `list` class object.
#' @param uid (string) the name of the uid column currently used data
#' @return The provided nested list with "uid" as UID.
convert_uids <- function(dll, uid) {
    dll <- lapply(dll,
        function(x, uid) {
            # Stop if UID isn't in the data frame
            if (!uid %in% colnames(x$"data")) {
                metasnf_error(
                    "UID column ", uid, " is not present in all data frames.",
                    env = 4
                )
            }
            colnames(x$"data")[colnames(x$"data") == uid] <- "uid"
            # Stop if UID isn't actually unique
            len_uids <- length(stats::na.omit(x$"data")$"uid")
            len_unique_uids <- length(unique(stats::na.omit(x$"data")$"uid"))
            if (len_uids != len_unique_uids) {
                metasnf_error(
                    "Column ", uid, " does not uniquely ID",
                    " all observations in at least one provided",
                    " data frame.",
                    env = 4
                )
            }
            return(x)
        },
        uid = uid
    )
    return(dll)
}

#' Remove observations with incomplete data from a data list-like list object
#'
#' Helper function during `data_list` class initialization. First applies
#' `stats::na.omit()` to the data frames named "data" within a nested list.
#' Then removes any observations that are not present across all data frames.
#'
#' @keywords internal
#' @param dll A data list-like `list` class object.
#' @return The provided dll with missing observations removed.
remove_dll_incomplete <- function(dll) {
    all_uids <- unique(unlist(lapply(dll, function(x) x$"data"$"uid")))
    uids_no_na <- lapply(dll, function(x) stats::na.omit(x$"data")$"uid")
    common_uids <- Reduce(intersect, uids_no_na)
    reduced_dll <- dll |>
        lapply(
            function(x) {
                x$"data"  <- x$"data" |>
                    dplyr::filter(
                        x$"data"$"uid" %in% common_uids
                    )
                x
            }
        )
    dropped_uids <- length(all_uids) - length(common_uids)
    if (dropped_uids > 0) {
        grammar <- if (dropped_uids == 1) "" else "s"
        metasnf_alert(
            dropped_uids, " observation", grammar, " dropped due to",
            " incomplete data."
        )
    }
    return(reduced_dll)
}

#' Add "uid_" prefix to all UID values in uid column
#'
#' @keywords internal
#' @param dll A data list-like `list` class object.
#' @return A data list with UIDs prefixed with the string "uid_".
prefix_dll_uid <- function(dll) {
    dll_prefixed <- lapply(
        dll,
        function(x) {
            x[[1]]$"uid" <- paste0("uid_", x[[1]]$"uid")
            return(x)
        }
    )
    return(dll_prefixed)
}

#' Sort data frames in a data list by their unique ID values
#'
#' @keywords internal
#' @param dll A data list-like `list` class object.
#' @return The data list-like object with all data frames sorted by uid.
arrange_dll <- function(dll) {
    data_objects <- lapply(dll, function(x) x[[1]])
    arranged_data_objects <- data_objects |>
        lapply(
            function(x) {
                dplyr::arrange(x, x$"uid")
            }
        )
    arranged_dll <- dll
    for (i in seq_along(dll)) {
        arranged_dll[[i]][[1]] <- arranged_data_objects[[i]]
    }
    return(arranged_dll)
}

#' Pull domains from a data list
#'
#' @keywords internal
#' @param dl A nested list of input data from `data_list()`.
#' @return A character vector of domains.
domains <- function(dl) {
    domain_list <- lapply(dl, function(x) x$"domain")
    return(domain_list)
}

#' Reorder the uids in a data list
#'
#' @keywords internal
#' @param dl A nested list of input data from `data_list()`.
#' @param ordered_uids A vector of the uid values in the data list
#'  in the desired order of the sorted data list.
#' @return A data list ("list"-class object) with reordered observations.
reorder_dl_uids <- function(dl, ordered_uids) {
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
#' @param dl A nested list of input data from `data_list()`.
#' @param name_mapping A named vector where the values are the features to be
#'  renamed and the names are the new names for those features.
#' @return A data list ("list"-class object) with adjusted feature names.
#' @export
#' @examples
#' dl <- data_list(
#'     list(pubertal, "pubertal_status", "demographics", "continuous"),
#'     list(anxiety, "anxiety", "behaviour", "ordinal"),
#'     list(depress, "depressed", "behaviour", "ordinal"),
#'     uid = "unique_id"
#' )
#'
#' summary(dl, "feature")
#'
#' name_changes <- c(
#'     "anxiety_score" = "cbcl_anxiety_r",
#'     "depression_score" = "cbcl_depress_r"
#' )
#'
#' dl <- rename_dl(dl, name_changes)
#'
#' summary(dl, "feature")
rename_dl <- function(dl, name_mapping) {
    dl_features <- summary(dl, "feature")$"name"
    mismatches <- which(!name_mapping %in% dl_features)
    if (length(mismatches) > 0) {
        metasnf_warning(
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

#' Make the uid UID columns of a data list first
#'
#' @keywords internal
#' @param dll A data list-like `list` class object.
#' @return The object with "uid" positioned as the first of each data frame
#'  column.
dll_uid_first_col <- function(dll) {
    dll <- lapply(
        dll,
        function(x) {
            x$"data" <- x$"data"[, unique(c("uid", colnames(x$"data")))]
            return(x)
        }
    )
}

#' Test if the object is a data list
#'
#' Given an object, returns `TRUE` if that object inherits from the `data_list`
#' class.
#'
#' @param x An object.
#' @return `TRUE` if the object inherits from the `data_list` class.
#' @export
is_data_list <- function(x) {
    inherits(x, "data_list")
}

#' Constructor for `data_list` class object
#'
#' @keywords internal
#' @param dll A data list-like `list` class object.
#' @return A `data_list` object, which is a nested list with class `data_list`.
new_data_list <- function(dll) {
    stopifnot(is.list(dll))
    stopifnot(is.list(dll[[1]]))
    dl <- structure(dll, class = c("data_list", "list"))
    # Define attributes
    # 1. UIDs of all observations
    attr(dl, "uids") <- dl[[1]]$"data"$"uid"
    # 2. Number of observations
    attr(dl, "n_observations") <- length(attributes(dl)$"uids")
    # 3. Stored features
    attr(dl, "features") <- dl |>
        as.data.frame() |>
        drop_cols("uid") |>
        colnames()
    # 4. Number of features
    attr(dl, "n_features") <- length(attributes(dl)$"features")
    # 5. Domains
    attr(dl, "domains") <- lapply(
        dl,
        function(x) {
            x$"domain"
        }
    ) |>
        unlist() |>
        as.character() |>
        unique()
    # 6. Number of domains
    attr(dl, "n_domains") <- length(attributes(dl)$"domains")
    # 7. Types
    attr(dl, "types") <- lapply(
        dl,
        function(x) {
            x$"type"
        }
    ) |>
        unlist() |>
        as.character() |>
        unique()
    # 8. Number of types
    attr(dl, "n_types") <- length(attributes(dl)$"types")
    # 9. Components
    attr(dl, "components") <- names(dl)
    # 10. Number of components
    attr(dl, "n_components") <- length(attributes(dl)$"components")
    return(dl)
}

#' Validator for data_list class object
#'
#' @keywords internal
#' @param dll A data list-like `list` class object.
#' @return If dll has a valid structure for a `data_list` class object,
#'  returns the input unchanged. Otherwise, raises an error.
validate_data_list <- function(dll) {
    class(dll) <- setdiff(class(dll), "data_list")
    # 1. Input is a list
    check_dll_inherits_list(dll)
    # 2. Input list stores 4-item lists
    check_dll_four_subitems(dll)
    # 3. Nested 4-items have proper names
    check_dll_subitem_names(dll)
    # 4. Nested 4-items have proper classes
    check_dll_subitem_classes(dll)
    # 5. Input has no duplicate components
    check_dll_duplicate_components(dll)
    # 6. Input has no duplicate features
    check_dll_duplicate_features(dll)
    # 7. Input has properly formatted UID columns
    check_dll_uid(dll)
    # 8. Input has valid types specified
    check_dll_types(dll)
    return(dll)
}

#' Check if data list contains any duplicate features
#'
#' @keywords internal
#' @param dll A data list-like `list` class object.
#' @return Doesn't return any value. Raises error if there are features with
#'  duplicate names in a generated data list.
check_dll_duplicate_features <- function(dll) {
    features <- dll |> lapply(
        function(x) {
            return(colnames(x$"data")[-1])
        }
    ) |>
        unlist() |>
        as.character()
    duplicates <- unique(features[duplicated(features)])
    if (length(duplicates) > 0) {
        metasnf_error(
            "Provided data cannot contain duplicate features.",
            env = 2
        )
    }
}

#' Check if data list contains any duplicate names
#'
#' @keywords internal
#' @param dll A data list-like `list` class object.
#' @return Doesn't return any value. Raises error if there are features with
#'  duplicate names in a generated data list.
check_dll_duplicate_components <- function(dll) {
    n_names <- length(names(dll))
    n_unique_names <- length(unique(names(dll)))
    if (n_names != n_unique_names) {
        metasnf_error(
            "Data list components must have unique names.",
            env = 2
        )
    }
}

#' Error if data list-like structure isn't a list
#'
#' @keywords internal
#' @param dll A data list-like `list` class object.
#' @return Raises error if data list-like structure isn't a list
check_dll_inherits_list <- function(dll) {
    if (!is.list(dll)) {
        metasnf_error(
            "Data list must inherit from class `list`.",
            env = 2
        )
    }
}

#' Error if data list-like list doesn't have only 4-item nested lists
#'
#' @keywords internal
#' @param dll A data list-like `list` class object.
#' @return Raises error if dll doesn't have only 4-item nested lists
check_dll_four_subitems <- function(dll) {
    if (!all(unlist(lapply(dll, length) == 4))) {
        metasnf_error(
            "Each data list component must be a 4-item list",
            " containing data (data.frame), name (character),",
            " domain (character), and type (character).",
            env = 3
        )
    }
}

#' Check valid item names for a data list-like list
#'
#' Error if data list-like structure doesn't have nested names of "data",
#' "name", "domain", and "type".
#'
#' @keywords internal
#' @param dll A data list-like `list` class object.
#' @return Raises error if dll doesn't have only 4-item nested lists
check_dll_subitem_names <- function(dll) {
    correct_names <- lapply(
        dll,
        function(x) {
            identical(names(x), c("data", "name", "domain", "type"))
        }
    ) |>
        unlist() |>
        all()
    if (!correct_names) {
        metasnf_error(
            "Each data list component must be a 4-item list",
            " containing data (data.frame), name (character),",
            " domain (character), and type (character).",
            env = 2
        )
    }
}

#' Check if UID columns in a nested list have valid structure for a data list
#'
#' @keywords internal
#' @param dll A data list-like `list` class object.
#' @return Raises an error if the UID columns do not have a valid structure.
check_dll_subitem_classes <- function(dll) {
    correct_subitem_classes <- lapply(
        dll,
        function(x) {
            all(
                c(
                    is.data.frame(x$"data"),
                    is.character(x$"name"),
                    is.character(x$"domain"),
                    is.character(x$"type")
                )
            )
        }
    ) |>
        unlist() |>
        all()
    if (!correct_subitem_classes) {
        metasnf_error(
            "Each data list component must be a 4-item list",
            " containing data (data.frame), name (character),",
            " domain (character), and type (character).",
            env = 2
        )
    }
}

#' Check if UID columns in a nested list have valid structure for a data list
#'
#' @keywords internal
#' @param dll A data list-like `list` class object.
#' @return Raises an error if the UID columns do not have a valid structure.
check_dll_uid <- function(dll) {
    # 1. Check if uid columns exist
    has_uids <- lapply(
        dll,
        function(x) {
            "uid" %in% colnames(x$"data")
        }
    ) |>
        unlist() |>
        all()
    if (!has_uids) {
        metasnf_error(
            "At least one included data frame is missing a `uid` column."
        )
    }
    uids <- lapply(
        dll,
        function(x) {
            x$"data"$"uid"
        }
    )
    first_uids <- uids[[1]]
    all_uids_match <- lapply(
        uids,
        function(x) {
            identical(first_uids, x)
        }
    ) |>
        unlist() |>
        all()
    at_least_one_uid <- length(first_uids) > 0
    unique_uid <- length(first_uids) == length(unique(first_uids))
    uid_vals_sw_uid <- all(startsWith(first_uids, "uid_"))
    valid_uids <- all(
        c(
            all_uids_match,
            at_least_one_uid,
            unique_uid,
            uid_vals_sw_uid
        )
    )
    if (!valid_uids) {
        metasnf_error(
            "All data frames must contain identical `uid` columns",
            " that uniquely identify all observations.",
            env = 2
        )
    }
}

#' Error if data list-like structure has invalid feature types
#'
#' @keywords internal
#' @param dll A data list-like `list` class object.
#' @return Raises an error if the loaded types are not among continuous,
#'  discrete, ordinal, categorical, or mixed.
check_dll_types <- function(dll) {
    valid_dll_types <- lapply(
        dll,
        function(x) {
            x$"type" %in% c(
                "continuous",
                "discrete",
                "ordinal",
                "categorical",
                "mixed"
            )
        }
    ) |>
        unlist() |>
        all()
    if (!valid_dll_types) {
        metasnf_error(
            "Valid component types include continuous, discrete,",
            " ordinal, categorical, and mixed.",
            env = 2
        )
    }
}

#' Error if empty input provided during data list initialization
#'
#' @keywords internal
#' @param data_list_input Input data provided for data list initialization.
#' @return Raises an error if data_list_input has 0 length.
check_dll_empty_input <- function(data_list_input) {
    if (length(data_list_input) == 0) {
        metasnf_error(
            "Data list initialization requires at least one input.",
            " See `?data_list` for more examples.",
            env = 2
        )
    }
}

#' Apply-like function for data list objects
#'
#' This function enables manipulating a `data_list` class object with lapply
#' syntax without removing that object's `data_list` class attribute. The
#' function will only preserve this attribute if the result of the apply call
#' has a valid data list structure.
#'
#' @param X A `data_list` class object.
#' @param FUN The function to be applied to each data list component.
#' @param ... Optional arguments to `FUN`.
#' @return If FUN applied to each component of X yields a valid data list, a
#'  data list. Otherwise, a list.
#' @export
#' @examples
#' # Convert all UID values to lowercase
#' dl <- data_list(
#'     list(abcd_income, "income", "demographics", "discrete"),
#'     list(abcd_colour, "colour", "likes", "categorical"),
#'     uid = "patient"
#' )
#'
#' dl_lower <- dlapply(
#'     dl,
#'     function(x) {
#'         x$"data"$"uid" <- tolower(x$"data"$"uid")
#'         return(x)
#'     }
#' )
dlapply <- function(X, FUN, ...) {
    if (!(inherits(X, "list"))) {
        metasnf_error(
            "`dlapply` can only be used with list-class objects."
        )
    }
    result <- base::lapply(X, FUN, ...)
    validation <- tryCatch(
        {
            result <- validate_data_list(result)
            result <- new_data_list(result)
            result
        },
        error = function(e) {
            metasnf_warning(
                "Result could not be coerced into class `data_list`."
            )
            result
        }
    )
    return(validation)
}
