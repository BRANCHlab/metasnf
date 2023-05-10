#' Calculate distance matrices
#'
#' @description
#' Given a dataframe of numerical variables, return a euclidean distance matrix
#'
#' @param df Raw dataframe with subject IDs in column 1
#' @param input_type Either "numeric" (resulting in euclidean distances),
#'  "categorical" (resulting in binary distances), or "mixed" (resulting in
#'  gower distances)
#' @param scale Whether or not the data should be standard normalized prior to
#'  distance calculations
#'
#' @return dist_matrix Matrix of inter-observation distances
#'
#' @export
get_dist_matrix <- function(df, input_type, scale = FALSE) {
    # Move subject keys into dataframe rownames
    df <- data.frame(df, row.names = 1)
    if (input_type == "numeric") {
        if (scale) {
            df <- SNFtool::standardNormalization(df)
        }
        dist_matrix <- as.matrix(stats::dist(df, method = "euclidean"))
    } else if (input_type %in% c("mixed", "categorical")) {
        df <- abcdutils::char_to_fac(df)
        dist_matrix <-
            as.matrix(cluster::daisy(df, metric = "gower", warnBin = FALSE))
    } else {
        rlang::abort(
            paste0("The value ", input_type, " is not a valid input type."),
            class = "invalid_input")
    }
    return(dist_matrix)
}

#' Build a design matrix - but softcoded
#'
#' @param data_list a data list object to determine variables for inclusion/exclusion
#' @param nrows number of design matrix rows
#' @param seed set a seed for the random matrix generation. Note that this
#' @param retry_limit The maximum number of attempts to generate a novel row
#'  affects the global seed.
#'
#' @return design_matrix A design matrix
#'
#' @export
build_design_matrix <- function(data_list, nrows = 0, seed = NULL, retry_limit = 10) {
    if (!is.null(seed)) {
        set.seed(seed)
        print("The global seed has been changed!")
    }
    dm_cols <- c(
        "row_id",
        paste0("inc_", sdl(data_list)$"name"),
        "snf_scheme",
        "eigen_or_rot",
        "K",
        "alpha")
    design_matrix_base <- as.data.frame(matrix(0, ncol = length(dm_cols), nrow = 0))
    colnames(design_matrix_base) <- dm_cols
    design_matrix <- add_design_matrix_rows(design_matrix_base, nrows)
    return(design_matrix)
}

#' Generate random removal sequence
#'
#' Helper function to contribute to rows within the design matrix.
#' Number of columns removed follows the exponential probability distribution
#' to typically keep all or most columns.
#'
#' @param num_cols Number of feature columns in consideration for exclusion
#'
#' @return shuffled_removals Binary vector sequence indicating if a column
#' should be included (1) or excluded (0)
#'
#' @export
random_removal <- function(num_cols) {
    rand_vals <- stats::rexp(10000)
    rand_vals <- rand_vals / max(rand_vals) * num_cols
    rand_vals <- floor(rand_vals)
    num_removed <- sample(rand_vals, 1)
    remove_placeholders <- rep(0, num_removed)
    keep_placeholders <- rep(1, num_cols - num_removed)
    unshuffled_removals <- c(remove_placeholders, keep_placeholders)
    shuffled_removals <- sample(unshuffled_removals)
    return(shuffled_removals)
}

#' Add design matrix rows
#'
#' @param design_matrix The existing design matrix
#' @param nrows The number of rows to be added to the design matrix
#' @param retry_limit The maximum number of attempts to generate a novel row
#'
#' @return design_matrix New design matrix containing additional rows
#'
#' @export
add_design_matrix_rows <- function(design_matrix, nrows, retry_limit = 10) {
    i <- 0
    num_retries <- 0
    while (i < nrows) {
        row_id <- nrow(design_matrix) + 1
        new_row <- vector()
        # Inclusion columns
        num_inclusion_cols <- sum(startsWith(colnames(design_matrix), "inc"))
        inclusions <- t(data.frame(random_removal(num_inclusion_cols)))
        inclusion_names <-
            colnames(design_matrix)[startsWith(colnames(design_matrix), "inc")]
        colnames(inclusions) <- inclusion_names
        # Other free parameters
        snf_scheme <- sample(1:3, 1)
        eigen_or_rot <- sample(1:2, 1)
        # K and alpha range based on prior hyperparameter scans
        K <- sample(10:30, 1)
        alpha <- (sample(6:10, 1))/10
        # Putting it all together
        new_row <- cbind(
            row_id,
            inclusions,
            snf_scheme,
            eigen_or_rot,
            K,
            alpha)
        # Appending to design matrix
        colnames(new_row) <- colnames(design_matrix)
        new_row <- data.frame(new_row)
        design_matrix <- rbind(design_matrix, new_row)
        i <- i + 1
        # Check if newly added row already exists
        dm_no_id <- design_matrix[, 2:length(design_matrix)]
        num_duplicates <- length(which(
            duplicated(dm_no_id) |
            duplicated(dm_no_id, fromLast = TRUE)))
        if (num_duplicates > 0) {
            i <- i - 1
            design_matrix <- design_matrix[seq_len(nrow(design_matrix)) - 1, ]
            num_retries <- num_retries + 1
        } else {
            num_retries <- 0
        }
        # Limit how many times a new row ended up already existing
        if (num_retries > retry_limit) {
            break
        }
    }
    if (num_retries > retry_limit) {
       print("Matrix row building aborted.")
       print("To keep adding rows, try raising the retry_limit parameter.")
    }
    row.names(design_matrix) <- NULL
    return(design_matrix)
}

#' Build design matrix for scanning alpha and K
#'
#' @return design_matrix Adds the standard grid expansion for SNF hyperparams
#'
#' @export
build_design_matrix_ak <- function() {
    design_matrix <- build_design_matrix()
    design_matrix[1:80, ] <- 1
    hyperparam_grid <- expand.grid(1:10, 3:10)
    colnames(hyperparam_grid) <- c("K", "alpha")
    design_matrix$K <- hyperparam_grid$K * 10
    design_matrix$alpha <- hyperparam_grid$alpha / 10
    return(design_matrix)
}

#' Generate data_list object - but softcoded
#'
#' This is the major data object that will be processed when iterating through
#'  the design matrix. The full list contains one list per measurement type.
#'  Within each measurement type's list, elements include the actual data
#'  structure, the name, the domain, and the data 'type' (i.e, numeric or
#'  categorical).
#' 
#' To-do: include checks to make sure format of data list is correct
#'
#' @param ... Lists formatted as (df, "df_name", "df_domain", "df_type")
#' @param old_uid (string) the name of the uid column currently used data
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
#'     old_uid = "patient_id"
#' )
generate_data_list <- function(..., old_uid = NULL) {
    # The object that will contain all the data
    data_list <- list(...)
    # Assign names to the nested list elements
    data_list_names <- c("data", "name", "domain", "type")
    data_list <- lapply(data_list, stats::setNames, data_list_names)
    data_list <- convert_uids(data_list, old_uid)
    return(data_list)
}

#' Convert unique identifiers of data_list to 'subjectkey'
#'
#' Column name "subjectkey" is reserved for the unique identifier of subjects.
#'  This function ensures all dataframes have their UID set as "subjectkey".
#'
#' @param data_list a data_list
#' @param old_uid (string) the name of the uid column currently used data
#'
#' @return dl_renamed_id data list with 'subjectkey' as UID
#'
#' @export
convert_uids <- function(data_list, old_uid = NULL) {
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
    #  there was no old_uid specified.
    if (is.null(old_uid)) {
        stop(paste0(
            "Please specify parameter 'old_uid' with the name of the column",
            " currently used as each row's unique identifier. This row will",
            " be converted to 'subjectkey' for the remaining metasnf analyses."
        ))
    }
    # Check to ensure that the user specified UID exists in the data_list
    if (!old_uid %in% d1_cols) {
        stop(paste0(
            "The specified original UID (", old_uid, ") is not present in",
            " this data list. Are you sure you spelled it correctly?"
        ))
    }
    # Convert the user specified original UID to 'subjectkey'
    dl_renamed_id <- lapply(data_list,
        function(x) {
            colnames(x$"data")[colnames(x$"data") == old_uid] <- "subjectkey"
            x
        }
    )
    print("UID successfully converted to subjectkey.")
    return(dl_renamed_id)
}

#' Generate outcome_list object - but softcoded
#'
#' The major object containing all outcome variables
#'
#' @param ... Lists of outcomes formatted as (dataframe, "name", "type")
#'
#' @return outcome_list structure containing all outcome measure data
#'
#' @export
generate_outcome_list <- function(...) {
    # The object that will contain all the data
    outcome_list <- list(...)
    # Assign names to the nested list elements
    outcome_list_names <- c("data", "name", "type")
    outcome_list <- lapply(outcome_list, stats::setNames, outcome_list_names)
    return(outcome_list)
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

#' SNF a data_list
#'
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#' @param scheme Which SNF system to use to achieve the final fused network
#' @param K K hyperparameter
#' @param alpha alpha/eta/sigma hyperparameter
#'
#' @return fused_network The final fused network for clustering
#'
#' @export
snf_step <- function(data_list, scheme, K = 20, alpha = 0.5) {
    # Subset just to those patients who are common in all inputs
    data_list <- data_list |>
        reduce_dl_to_common() |>
        arrange_dl()
    # Remove NAs function can go here later
    # The individual scheme creates similarity matrices for each dl element
    #  and pools them all into a single SNF run
    if (scheme %in% c("individual", 1)) { # This is functioning properly
        dist_list <- lapply(data_list,
            function(x) {
                get_dist_matrix(df = x$"data", input_type = x$"type")
            })
        sim_list <- lapply(dist_list,
            function(x) {
                SNFtool::affinityMatrix(x, K = K, sigma = alpha)
            })
        fused_network <- SNFtool::SNF(sim_list, K = K)
    # The domain scheme first runs domain merge on the data list (concatenates
    #  any data of the same domain) and then pools the concatenated data into a
    #  single SNF run
    } else if (scheme %in% c("domain", 2)) { # This works
        data_list <- domain_merge(data_list)
        dist_list <- lapply(data_list,
            function(x) {
                get_dist_matrix(df = x$"data", input_type = x$"type",
                    scale = TRUE)
            })
        sim_list <- lapply(dist_list,
            function(x) {
                SNFtool::affinityMatrix(x, K = K, sigma = alpha)
            })
        fused_network <- SNFtool::SNF(sim_list, K = K)
    # The twostep scheme
    } else if (scheme %in% c("twostep", 3)) {
        fused_network <- two_step_merge(data_list)
    } else {
        rlang::abort(
            paste0("The value '", scheme, "' is not a valid snf scheme."),
            class = "invalid_input")
    }
    return(fused_network)
}


#' Execute variations of SNF as described by a design matrix - but softcoded
#'
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#' @param design_matrix matrix indicating parameters to iterate SNF through
#'
#' @return populated_design_matrix design matrix with filled columns related to
#'  subtype membership
#'
#' @export
execute_design_matrix_soft <- function(data_list, design_matrix) {
    start <- Sys.time()
    design_matrix <- data.frame(design_matrix)
    subjects <- data_list[[1]]$"data"$"subjectkey"
    output_matrix <- add_char_vec_as_cols(design_matrix, subjects, 0)
    # Iterate through the rows of the design matrix
    remaining_seconds_vector <- vector()
    for (i in seq_len(nrow(design_matrix))) {
        start_time <- Sys.time()
        dm_row <- design_matrix[i, ]
        current_data_list <- execute_inclusion(data_list, dm_row)
        # Execute the current row's SNF scheme
        current_snf_scheme <- dplyr::case_when(
            dm_row$"snf_scheme" == 1 ~ "individual",
            dm_row$"snf_scheme" == 2 ~ "domain",
            dm_row$"snf_scheme" == 3 ~ "twostep",
        )
        K <- design_matrix[i, "K"]
        alpha <- design_matrix[i, "alpha"]
        fused_network <- snf_step(
            current_data_list,
            current_snf_scheme,
            K = K,
            alpha = alpha)
        all_clust <- SNFtool::estimateNumberOfClustersGivenGraph(fused_network)
        # Execute the current row's clustering
        if (dm_row$"eigen_or_rot" == 1) {
            eigen_best <- all_clust$`Eigen-gap best`
            nclust <- eigen_best
        } else if (dm_row$"eigen_or_rot" == 2) {
            rot_best <- all_clust$`Rotation cost best`
            nclust <- rot_best
        } else {
            # To-do: move this into design matrix generation or earlier in
            #  this function
            rlang::abort(
                paste0(
                    "The eigen_or_rot value ", dm_row$"eigen_or_rot", " is not",
                    "a valid input type."), class = "invalid_input")
        }
        cluster_results <- SNFtool::spectralClustering(fused_network, nclust)
        # Assign subtype membership
        output_matrix[i, rownames(fused_network)] <- cluster_results
        end_time <- Sys.time()
        seconds_per_row <- as.numeric(end_time - start_time)
        rows_remaining <- nrow(design_matrix) - i
        remaining_seconds_vector <- c(remaining_seconds_vector, seconds_per_row)
        if (length(remaining_seconds_vector) > 10) {
            remaining_seconds_vector <-
                remaining_seconds_vector[2:length(remaining_seconds_vector)]
        }
        remaining_seconds <-
            round(mean(remaining_seconds_vector) * rows_remaining, 0)
        print(
            paste0(
                "Row: ", i, "/", nrow(design_matrix),
                " | ",
                "Time remaining: ",
                remaining_seconds,
                " seconds"))
    }
    # Add number of clusters to output matrix
    output_matrix <- output_matrix |>
        dplyr::mutate(nclust = dplyr::case_when(
            eigen_or_rot == 1 ~ eigen_best,
            eigen_or_rot == 2 ~ rot_best),
            .keep = "unused") |>
        unique()
    end <- Sys.time()
    print(end - start)
    output_matrix <- abcdutils::col_to_num_all_possible(output_matrix)
    return(output_matrix)
}



#' Execute variations of SNF as described by a design matrix
#'
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#' @param design_matrix matrix indicating parameters to iterate SNF through
#' @param outcome_list nested list of outcome data
#'
#' @return populated_design_matrix design matrix with filled columns related to
#'  subtype membership and cluster performance
#'
#' @export
execute_design_matrix <- function(data_list, design_matrix, outcome_list) {
    start <- Sys.time()
    design_matrix <- data.frame(design_matrix)
    output_matrix <- build_output_matrix(data_list, design_matrix)
    # Iterate through the rows of the design matrix
    remaining_seconds_vector <- vector()
    for (i in seq_len(nrow(design_matrix))) {
        start_time <- Sys.time()
        dm_row <- design_matrix[i, ]
        current_data_list <- execute_inclusion(data_list, dm_row)
        # Execute the current row's SNF scheme
        current_snf_scheme <- dplyr::case_when(
            dm_row$"snf_scheme" == 1 ~ "individual",
            dm_row$"snf_scheme" == 2 ~ "domain",
            dm_row$"snf_scheme" == 3 ~ "twostep",
        )
        K <- design_matrix[i, "K"]
        alpha <- design_matrix[i, "alpha"]
        fused_network <- snf_step(
            current_data_list,
            current_snf_scheme,
            K = K,
            alpha = alpha)
        all_clust <- SNFtool::estimateNumberOfClustersGivenGraph(fused_network)
        eigen_best <- all_clust$`Eigen-gap best`
        rot_best <- all_clust$`Rotation cost best`
        output_matrix[i, ]$"eigen_best" <- eigen_best
        output_matrix[i, ]$"rot_best" <- rot_best
        # Execute the current row's clustering
        if (dm_row$"eigen_or_rot" == 1) {
            nclust <- eigen_best
        } else if (dm_row$"eigen_or_rot" == 2) {
            nclust <- rot_best
        } else {
            rlang::abort(
                paste0(
                    "The eigen_or_rot value ", dm_row$"eigen_or_rot", " is not",
                    "a valid input type."), class = "invalid_input")
        }
        cluster_results <- SNFtool::spectralClustering(fused_network, nclust)
        # Assign subtype membership
        output_matrix[i, rownames(fused_network)] <- cluster_results
        # Pull clustered subjects in a useful way for outcome evaluation
        clustered_subs <- get_clustered_subs(output_matrix[i, ])
        assigned_subs <- clustered_subs |>
            dplyr::filter(clustered_subs$"cluster" != 0)
        # Assign p-values
        for (j in seq_along(outcome_list)) {
            current_outcome_component <- outcome_list[[j]]
            current_outcome_name <- current_outcome_component$"name"
            p_value <- get_p(assigned_subs, current_outcome_component)
            target_col <- grep(current_outcome_name, colnames(output_matrix))
            output_matrix[i, target_col] <- p_value
        }
        min_p <- get_min_p(output_matrix[i, ])
        mean_p <- get_mean_p(output_matrix[i, ])
        output_matrix[i, "min_p_val"] <- min_p
        output_matrix[i, "mean_p_val"] <- mean_p
        end_time <- Sys.time()
        seconds_per_row <- as.numeric(end_time - start_time)
        rows_remaining <- nrow(design_matrix) - i
        remaining_seconds_vector <- c(remaining_seconds_vector, seconds_per_row)
        if (length(remaining_seconds_vector) > 10) {
            remaining_seconds_vector <-
                remaining_seconds_vector[2:length(remaining_seconds_vector)]
        }
        remaining_seconds <-
            round(mean(remaining_seconds_vector) * rows_remaining, 0)
        print(
            paste0(
                "Row: ", i, "/", nrow(design_matrix),
                " | ",
                "Time remaining: ",
                remaining_seconds,
                " seconds"))
    }
    # Add number of clusters to output matrix
    output_matrix <- output_matrix |>
        dplyr::mutate(nclust = dplyr::case_when(
            eigen_or_rot == 1 ~ eigen_best,
            eigen_or_rot == 2 ~ rot_best),
            .keep = "unused") |>
        unique()
    end <- Sys.time()
    print(end - start)
    output_matrix <- abcdutils::col_to_num_all_possible(output_matrix)
    return(output_matrix)
}


#' Select p-values from output matrix
#'
#' @param output_matrix The output of execute_design_matrix()
#'
#' @return p_val_matrix P-values ready for heatmap plotting
#'
#' @export
p_val_select <- function(output_matrix) {
    p_val_matrix <- output_matrix |>
        dplyr::select(dplyr::ends_with("_p"), -c("min_p_val", "mean_p_val")) |>
        sapply(as.numeric) |>
        as.matrix()
    return(p_val_matrix)
}

#' Get minimum p-value
#'
#' @description
#' Given an output matrix row containing evaluated p-values, returns minimum
#'
#' @param output_matrix_row row of output_matrix object
#'
#' @return min_p minimum p-value
#'
#' @export
get_min_p <- function(output_matrix_row) {
    min_p <- output_matrix_row |>
        dplyr::mutate(dplyr::across(dplyr::ends_with("_p"), ~ as.numeric(.))) |>
        dplyr::select(dplyr::ends_with("_p")) |>
        min()
    return(min_p)
}

#' Get mean p-value
#'
#' @description
#' Given an output matrix row containing evaluated p-values, returns mean
#'
#' @param output_matrix_row row of output_matrix object
#'
#' @return mean_p mean p-value
#'
#' @export
get_mean_p <- function(output_matrix_row) {
    mean_p <- output_matrix_row |>
        dplyr::mutate(dplyr::across(dplyr::ends_with("_p"), ~ as.numeric(.))) |>
        dplyr::select(dplyr::ends_with("_p")) |>
        rowMeans()
    return(mean_p)
}

#' Get p-value
#'
#' @description
#' Depending on outcome measure, perform ordinal regression or linear regression
#'  and return p-value as a benchmark measure of how well-separated clusters
#'  are by the outcome measure
#'
#' @param assigned_subs dataframe of subjects who were assigned to a cluster and
#'  the cluster they were assigned to
#' @param outcome_component the outcome_list element of interest
#'
#' @return p_val the smallest p-value of interest
#'
#' @export
get_p <- function(assigned_subs, outcome_component) {
    outcome <- outcome_component$"data"
    outcome_name <- outcome_component$"name"
    outcome_type <- outcome_component$"type"
    if (outcome_type == "ordinal") {
        p_val <- ord_reg_p(assigned_subs, outcome, outcome_name)
    } else if (outcome_type == "numeric") {
        p_val <- lin_reg_p(assigned_subs, outcome, outcome_name)
    } else {
        stop(paste0(
            "Unsupported outcome type: ",
            outcome_type,
            ". Accepted types for now are numeric and ordinal."
        ))
    }
    return(p_val)
}

#' Execute inclusion
#'
#' @description
#' Given a data list and a design matrix row, returns a data list of selected
#'  inputs
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#' @param design_matrix matrix indicating parameters to iterate SNF through
#'
#' @return selected_data_list
#'
#' @export
execute_inclusion <- function(data_list, design_matrix) {
# Dataframe just of the inclusion variables
    inc_df <- design_matrix |>
        dplyr::select(dplyr::starts_with("inc"))
    # The subset of columns that are in 'keep' (1) mode
    keepcols <- colnames(inc_df)[inc_df[1, ] == 1]
    # The list of data_list elements that are to be selected
    in_keeps_list <- lapply(data_list,
        function(x) {
            paste0("inc_", x$"name") %in% keepcols
        }) # Converting to a logical type to do the selection
    in_keeps_log <- c(unlist(in_keeps_list))
    # The selection
    selected_dl <- data_list[in_keeps_log]
    reduced_selected_dl <- reduce_dl_to_common(selected_dl)
    return(reduced_selected_dl)
}

#' Summarize data list
#'
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#'
#' @return dl_summary Summarized output
#'
#' @export
sdl <- function(data_list) {
    dl_summary <-
        data.frame(
            name = unlist(lapply(data_list, function(x) x$"name")),
            type = unlist(lapply(data_list, function(x) x$"type")),
            domain = unlist(domains(data_list)),
            length = unlist(lapply(data_list, function(x) dim(x$"data")[1])),
            width = unlist(lapply(data_list, function(x) dim(x$"data")[2])))
    return(dl_summary)
}

#' Summarize data list
#'
#' @param outcome_list an outcome_list
#'
#' @return ol_summary Summarized output
#'
#' @export
sol <- function(outcome_list) {
    ol_summary <-
        data.frame(
            name = unlist(lapply(outcome_list, function(x) x$"name")),
            type = unlist(lapply(outcome_list, function(x) x$"type")),
            length = unlist(lapply(outcome_list, function(x) dim(x$"data")[1])),
            width = unlist(lapply(outcome_list, function(x) dim(x$"data")[2])))
    return(ol_summary)
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

#' Domain merge
#'
#' @description
#' Given a data_list, returns a new data_list where all original data objects of
#'  a particlar domain have been concatenated
#'
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#'
#' @return domain_dl
#'
#' @export
domain_merge <- function(data_list) {
    domain_dl <- list()
    for (i in seq_along(data_list)) {
        current_component <- data_list[[i]]
        current_domain <- data_list[[i]]$"domain"
        if (length(domain_dl) == 0) {
            domain_dl <- append(domain_dl, list(current_component))
        } else if (current_domain %in% domains(domain_dl)) {
            existing_match_pos <- which(domains(domain_dl) == current_domain)
            existing_component <- domain_dl[[existing_match_pos]]
            existing_match_data <- existing_component$"data"
            data_to_merge <- current_component$"data"
            merged_data <- dplyr::inner_join(
                existing_match_data, data_to_merge, by = "subjectkey")
            merged_component <- existing_component
            merged_component$"data" <- merged_data
            merged_component$"name" <-
                paste0("merged_", merged_component$"domain")
            merged_component$"type" <- dplyr::case_when(
                existing_component$"type" == current_component$"type" ~
                    current_component$"type",
                existing_component$"type" != current_component$"type" ~
                    "mixed"
            )
            domain_dl[[existing_match_pos]] <- merged_component
        } else {
            domain_dl <- append(domain_dl, list(current_component))
        }
    }
    return(domain_dl)
}

#' Two step SNF
#'
#' @description
#' Individual dataframes into individual similarity matrices into one fused
#'  network per domain into one final fused network.
#'
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#' @param K K hyperparameter
#' @param alpha alpha/eta/sigma hyperparameter
#'
#' @return fused_network The final fused network for clustering
#'
#' @export
two_step_merge <- function(data_list, K = 20, alpha = 0.5) {
    dist_list <- lapply(data_list,
        function(x) {
            get_dist_matrix(df = x$"data", input_type = x$"type")
        })
    sim_list <- lapply(dist_list,
        function(x) {
            SNFtool::affinityMatrix(x, K = K, sigma = alpha)
        })
    affinity_list <- data_list
    for (i in seq_along(affinity_list)) {
        affinity_list[[i]]$"data" <- sim_list[[i]]
    }
    affinity_unique_dl <- list()
    unique_domains <- unique(unlist(domains(affinity_list)))
    for (i in seq_along(unique_domains)) {
        affinity_unique_dl <- append(affinity_unique_dl, list(list()))
    }
    names(affinity_unique_dl) <- unique_domains
    for (i in seq_along(affinity_list)) {
        al_current_domain <- affinity_list[[i]]$"domain"
        al_current_amatrix <- affinity_list[[i]]$"data"
        audl_domain_pos <- which(names(affinity_unique_dl) == al_current_domain)
        affinity_unique_dl[[audl_domain_pos]] <-
            append(affinity_unique_dl[[audl_domain_pos]],
            list(al_current_amatrix))
    }
    # Fusing individual matrices into domain affinity matrices
    step_one <- lapply(affinity_unique_dl,
       function(x) {
           if (length(x) == 1) {
               x[[1]]
           } else {
               SNFtool::SNF(x, K = K)
           }
       })
    # Fusing domain affinity matrices into final fused network
    if (length(step_one) > 1) {
        fused_network <- SNFtool::SNF(step_one, K = K)
    } else {
        fused_network <- step_one[[1]]
    }
    return(fused_network)
}

#' Build output matrix
#'
#' @description
#' The matrix that will contain all the columns to be populated by SNF results
#'
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#' @param design_matrix matrix indicating parameters to iterate SNF through
#'
#' @return output_matrix The output matrix
#'
#' @export
build_output_matrix <- function(data_list, design_matrix) {
    subjects <- list()
    for (i in seq_along(data_list)) {
        subjects <- append(subjects, (data_list[[i]][[1]]$"subjectkey"))
    }
    subjects <- unique(unlist(subjects))
    output_matrix <- add_char_vec_as_cols(design_matrix, subjects, 0)
    other_cols <- c(
        "eigen_best",
        "rot_best",
        "cbcl_nausea_p",
        "cbcl_vomiting_p",
        "cbcl_dizzy_p",
        "cbcl_overtired_p",
        "cbcl_sleeping_more_p",
        "cbcl_sleeping_less_p",
        "cbcl_depress_p",
        "cbcl_anxiety_p",
        "cbcl_attention_p",
        "cbcl_aggressive_p",
        "min_p_val",
        "mean_p_val")
    output_matrix <- add_char_vec_as_cols(output_matrix, other_cols, 0)
    return(output_matrix)
}

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

#' Get clustered subjects
#'
#' @description
#' Pull a dataframe of clustered subjects from an output matrix structure
#'
#' @param output_matrix_row Output matrix row containing subtype membership
#'
#' @return clustered_subs Dataframe
#'
#' @export
get_clustered_subs <- function(output_matrix_row) {
    clustered_subs <-
        data.frame(t(output_matrix_row[1,
                     which(startsWith(colnames(output_matrix_row), "NDAR"))]))
    clustered_subs$"subjectkey" <- rownames(clustered_subs)
    rownames(clustered_subs) <- NULL
    clustered_subs <- clustered_subs |>
        dplyr::select("subjectkey", dplyr::starts_with("X")) |>
        dplyr::rename("cluster" = dplyr::starts_with("X"))
    return(clustered_subs)
}

#' Ordinal regression p-value
#'
#' @description
#' Returns the p-value following an ordinal regression in which cluster
#'  is the IV and a provided ordinal variable is the DV
#'
#' @param clust_membership Dataframe of cluster membership (get_clustered_subs)
#' @param outcome_df Dataframe containing DV
#' @param outcome_var DV as a string
#'
#' @return p_val The overall p-value distinguishing clusters by the DV
#'
#' @export
ord_reg_p <- function(clust_membership, outcome_df, outcome_var) {
    merged_df <-
        dplyr::inner_join(clust_membership, outcome_df, by = "subjectkey")
    num_classes <- length(unique(merged_df[, outcome_var]))
    # If there are only 2 tiers to the ordinal scale, just use linear model
    if (num_classes == 2) {
        return(lin_reg_p(clust_membership, outcome_df, outcome_var))
    }
    merged_df$"cluster" <- as.factor(merged_df$"cluster")
    merged_df[, outcome_var] <- as.ordered(merged_df[, outcome_var])
    null_model <- MASS::polr(merged_df[, outcome_var] ~ 1)
    full_model <- MASS::polr(merged_df[, outcome_var] ~ merged_df[, "cluster"])
    p_value <- stats::anova(null_model, full_model)$"Pr(Chi)"[2]
    return(p_value)
}

#' Linear regression p-value
#'
#' @description
#' Returns the p-value following an linear regression in which cluster
#'  is the IV and a provided ordinal variable is the DV
#'
#' @param clust_membership Dataframe of cluster membership (get_clustered_subs)
#' @param outcome_df Dataframe containing DV
#' @param outcome_var DV as a string
#'
#' @return p_val The overall p-value distinguishing clusters by the DV
#'
#' @export
lin_reg_p <- function(clust_membership, outcome_df, outcome_var) {
    merged_df <-
        dplyr::inner_join(clust_membership, outcome_df, by = "subjectkey")
    merged_df$"cluster" <- as.factor(merged_df$"cluster")
    model <- stats::lm(merged_df[, outcome_var] ~ merged_df[, "cluster"])
    fstat <- summary(model)$"fstatistic"
    p <- stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
    attributes(p) <- NULL
    return(p)
}
