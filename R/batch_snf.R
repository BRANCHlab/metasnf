#' Run variations of SNF as described by a settings matrix
#'
#' @param data_list nested list of input data. See ?get_data_list
#' @param settings_matrix matrix indicating parameters to iterate SNF through
#' @param processes Specify number of processes used to complete SNF iterations
#'     * `1` (default) Sequential processing: function will iterate through the
#'       `settings_matrix` one row at a time with a for loop. This option will
#'       not make use of multiple CPU cores, but will show a progress bar.
#'     * `2` or higher: Parallel processing will use the
#'       `future.apply::future_apply` to distribute the SNF iterations across
#'       the specified number of CPU cores. If higher than the number of
#'       available cores, a warning will be printed and the maximum number of
#'       cores will be used.
#'     * `max`: All available cores will be used.
#' @param affinity_matrix_dir If specified, this directory will be used to save
#'  all generated affinity matrices
#' @param clust_algs_list List of custom clustering algorithms to apply
#'  to the final fused network. See ?generate_clust_algs_list
#' @param run_clustering If TRUE (default), will apply default or custom
#'  clustering algorithms to provide cluster solutions on every iteration of
#'  SNF. If FALSE, parameter `affinity_matrix_dir` must be specified.
#'
#'
#' @return populated_settings_matrix settings matrix with filled columns
#' related to subtype membership
#'
#' @export
batch_snf <- function(data_list,
                      settings_matrix,
                      processes = 1,
                      affinity_matrix_dir = NULL,
                      clust_algs_list = NULL,
                      run_clustering = TRUE) {
    ###########################################################################
    # 1. Checking compatibility of settings
    ###########################################################################
    # The user may have chosen to simultaneously not save affinity matrices and
    #  to not apply any clustering algorithms. In that case, this function is
    #  not really doing anything. Stop the function with an error.
    if (is.null(affinity_matrix_dir) & !run_clustering) {
        stop(
            paste0(
               "batch_snf has been called with the run_clustering parameter",
               " set to FALSE (no clustering will occur) and no path provided",
               " in the affinity_matrix_dir parameter for storing matrices.",
               " With this combination of settings, the batch_snf function",
               " yields no meaningful output."
            )
        )
    }
    ###########################################################################
    # 2. Call separate function for parallel processing
    ###########################################################################
    if (processes != 1) {
        available_cores <- future::availableCores()[["system"]]
        # Use all available cores
        if (processes == "max") {
            om <- parallel_batch_snf(
                data_list = data_list,
                settings_matrix = settings_matrix,
                processes = available_cores
            )
            return(om)
        # Use the user-specified number of cores
        } else if (is.numeric(processes)) {
            if (processes > available_cores) {
                warning("More processes than cores available specified.")
                print(
                    paste0(
                        "You specified ", processes, " processes, but only ",
                        available_cores, " cores are available. Defaulting to ",
                        available_cores, " processes."
                    )
                )
                processes <- available_cores
            }
            om <- parallel_batch_snf(
                data_list = data_list,
                settings_matrix = settings_matrix,
                processes = processes
            )
            return(om)
        # Invalid input check
        } else {
            stop("Invalid number of processes specified.")
        }
    }
    ###########################################################################
    # 3. Start timer to keep track of entire function duration
    ###########################################################################
    start <- proc.time() # used to track time taken for entire function
    remaining_seconds_vector <- vector() # used to estimate time to completion
    ###########################################################################
    # 4. Ensure settings_matrix is a data.frame (not a tibble or matrix)
    ###########################################################################
    settings_matrix <- data.frame(settings_matrix)
    ###########################################################################
    # 5. Creation of solutions_matrix (where clustering results are stored)
    # solutions_matrix is a dataframe with the following columns:
    #  - row_id (1 column): number matching the row of the settings_matrix used
    #    to generate this solution
    #  - inc_* (1 column per input df): Binary indicating if the input df was
    #    incuded (1) or excluded (0) for this row of SNF
    #  - snf_scheme (1 column): number indicating which of the preprogrammed
    #    'schemes' was used to for this run of SNF
    #  - alpha (AKA sigma or eta): value of affinity matrix hyperparameter
    #  - K: value of affinity matrix hyperparameter
    #  - T: Number of iterations of SNF
    #  - subject_* (1 column per patient): cluster membership of that patient
    #    for that row. Only included when run_clustering = TRUE.
    #  - nclust (1 column): number of clusters in the cluster solution in that
    #    row. Only included when run_clustering = TRUE.
    ###########################################################################
    # `add_columns` extends a dataframe `df` with a column or vector of columns
    #  whose names are provided in the `newcols` parameter. The values in the
    #  newly added columns are specified in the `fill` parameter.
    ###########################################################################
    # 5a. solutions_matrix begins as the settings_matrix extended with one new
    #  column for every subjects.
    solutions_matrix <- add_columns(
        df = settings_matrix,
        newcols = data_list[[1]]$"data"$"subjectkey", # one column per patient
        fill = 0 # populate the new column with 0s by default
    )
    # 5b. solutions_matrix gets one new column to store the cluster that each
    #  subject was assigned to.
    solutions_matrix <- add_columns(
        df = settings_matrix,
        newcols = "nclust",
        fill = 0
    )
    ###########################################################################
    # 6. Iterate through the rows of the settings matrix
    ###########################################################################
    for (i in seq_len(nrow(settings_matrix))) {
        start_time <- Sys.time() # used to estimate time to completion
        dm_row <- settings_matrix[i, ]
        current_data_list <- execute_inclusion(dm_row, data_list)
        # Apply the current row's SNF scheme
        current_snf_scheme <- dplyr::case_when(
            dm_row$"snf_scheme" == 1 ~ "individual",
            dm_row$"snf_scheme" == 2 ~ "domain",
            dm_row$"snf_scheme" == 3 ~ "twostep",
        )
        K <- settings_matrix[i, "K"]
        alpha <- settings_matrix[i, "alpha"]
        # 4. Run SNF
        fused_network <- snf_step(
            current_data_list,
            current_snf_scheme,
            K = K,
            alpha = alpha)
        # 5. If user provided a path to save the affinity matrices, save them
        if (!is.null(affinity_matrix_dir)) {
            utils::write.csv(
                x = fused_network,
                file = affinity_matrix_path(affinity_matrix_dir, i),
                row.names = TRUE
            )
        }
        #######################################################################
        # 7. Clustering of the final fused network
        #######################################################################
        all_clust <- SNFtool::estimateNumberOfClustersGivenGraph(fused_network)
        # Use the current row's number of clusters heuristic
        if (dm_row$"eigen_or_rot" == 1) {
            eigen_best <- all_clust$`Eigen-gap best`
            nclust <- eigen_best
        } else if (dm_row$"eigen_or_rot" == 2) {
            rot_best <- all_clust$`Rotation cost best`
            nclust <- rot_best
        }
        solutions_matrix[i, "nclust"] <- nclust
        cluster_results <- SNFtool::spectralClustering(fused_network, nclust)
        # Assign subtype membership
        solutions_matrix[i, rownames(fused_network)] <- cluster_results
        # Print estimated time taken until function completion ################
        remaining_seconds_vector <- batch_snf_time_remaining(
            seconds_per_row = as.numeric(Sys.time() - start_time),
            rows_remaining = nrow(settings_matrix) - i,
            row = i,
            remaining_seconds_vector
        )
    }
    # Add number of clusters to solutions matrix #################################
    solutions_matrix <- solutions_matrix |>
        unique()
    solutions_matrix <- numcol_to_numeric(solutions_matrix)
    # Print total time taken for function completion ##########################
    total_time <- (proc.time() - start)[["elapsed"]]
    print(paste0("Total time taken: ", round(total_time, 0), " seconds."))
    # Return solutions matrix #################################################
    return(solutions_matrix)
}

#' Parallel processing form of batch_snf
#'
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#' @param settings_matrix matrix indicating parameters to iterate SNF through
#' @param processes Number of parallel processes used when executing SNF
#'
#' @return populated_settings_matrix settings matrix with filled columns related to
#'  subtype membership
#'
#' @export
parallel_batch_snf <- function(data_list,
                               settings_matrix,
                               processes) {
    print(
        paste0(
            "Utilizing ", processes, " processes. Real time progress is not",
            " available during parallel processing."
        )
    )
    start <- proc.time()
    future::plan(future::multisession, workers = processes)
    settings_matrix <- data.frame(settings_matrix)
    solutions_matrix <- future.apply::future_apply(
        settings_matrix,
        1,
        dm_row_fn,
        dl = data_list
    )
    solutions_matrix <- do.call("rbind", solutions_matrix)
    solutions_matrix <- solutions_matrix |>
        unique()
    solutions_matrix <- numcol_to_numeric(solutions_matrix)
    future::plan(future::sequential)
    total_time <- (proc.time() - start)[["elapsed"]]
    print(
        paste0(
            "Total time taken: ", total_time, " seconds."
        )
    )
    return(solutions_matrix)
}

#' Apply-based function for batch_snf
#'
#' @param dm_row a row of a settings matrix
#' @param dl a data list
#'
#' @return the corresponding OM row
#'
#' @export
dm_row_fn <- function(dm_row, dl) {
    dm_row <- data.frame(t(dm_row))
    current_data_list <- execute_inclusion(dm_row, dl)
    current_snf_scheme <- dplyr::case_when(
        dm_row$"snf_scheme" == 1 ~ "individual",
        dm_row$"snf_scheme" == 2 ~ "domain",
        dm_row$"snf_scheme" == 3 ~ "twostep",
    )
    K <- dm_row$"K"
    alpha <- dm_row$"alpha"
    fused_network <- snf_step(
        current_data_list,
        current_snf_scheme,
        K = K,
        alpha = alpha)
    all_clust <- SNFtool::estimateNumberOfClustersGivenGraph(fused_network)
    # Apply the current row's number of clusters heuristic
    if (dm_row$"eigen_or_rot" == 1) {
        eigen_best <- all_clust$`Eigen-gap best`
        nclust <- eigen_best
    } else if (dm_row$"eigen_or_rot" == 2) {
        rot_best <- all_clust$`Rotation cost best`
        nclust <- rot_best
    } else {
        # To-do: move this into settings matrix generation or earlier in
        #  this function
        rlang::abort(
            paste0(
                "The eigen_or_rot value ", dm_row$"eigen_or_rot", " is not",
                "a valid input type."), class = "invalid_input")
    }
    dm_row$"nclust" <- nclust
    cluster_results <- SNFtool::spectralClustering(fused_network, nclust)
    # Assign subtype membership
    dm_row[1, rownames(fused_network)] <- cluster_results
    return(dm_row)
}

#' Execute inclusion
#'
#' @description
#' Given a data list and a settings matrix row, returns a data list of selected
#'  inputs
#' @param settings_matrix matrix indicating parameters to iterate SNF through
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#'
#' @return selected_data_list
#'
#' @export
execute_inclusion <- function(settings_matrix, data_list) {
    # Dataframe just of the inclusion variables
    inc_df <- settings_matrix |>
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
        df <- char_to_fac(df)
        dist_matrix <-
            as.matrix(cluster::daisy(df, metric = "gower", warnBin = FALSE))
    } else {
        rlang::abort(
            paste0("The value ", input_type, " is not a valid input type."),
            class = "invalid_input")
    }
    return(dist_matrix)
}

#' SNF a data_list
#'
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#' @param scheme Which SNF system to use to achieve the final fused network
#' @param K K hyperparameter
#' @param alpha alpha/eta/sigma hyperparameter
#' @param t SNF number of iterations hyperparameter
#'
#' @return fused_network The final fused network for clustering
#'
#' @export
snf_step <- function(data_list, scheme, K = 20, alpha = 0.5, t = 20) {
    # Subset just to those patients who are common in all inputs
    data_list <- data_list |>
        reduce_dl_to_common() |>
        arrange_dl()
    # Remove NAs function can go here later
    # The individual scheme creates similarity matrices for each dl element
    #  and pools them all into a single SNF run
    if (scheme %in% c("individual", 1)) {
        dist_list <- lapply(data_list,
            function(x) {
                get_dist_matrix(df = x$"data", input_type = x$"type")
            })
        sim_list <- lapply(dist_list,
            function(x) {
                SNFtool::affinityMatrix(x, K = K, sigma = alpha)
            })
        fused_network <- SNFtool::SNF(Wall = sim_list, K = K, t = t)
    # The domain scheme first runs domain merge on the data list (concatenates
    #  any data of the same domain) and then pools the concatenated data into a
    #  single SNF run
    } else if (scheme %in% c("domain", 2)) {
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
        fused_network <- SNFtool::SNF(Wall = sim_list, K = K, t = t)
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
#' @param t SNF number of iterations hyperparameter
#'
#' @return fused_network The final fused network for clustering
#'
#' @export
two_step_merge <- function(data_list, K = 20, alpha = 0.5, t = 20) {
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
               SNFtool::SNF(Wall = x, K = K, t = t)
           }
       })
    # Fusing domain affinity matrices into final fused network
    if (length(step_one) > 1) {
        fused_network <- SNFtool::SNF(Wall = step_one, K = K, t = t)
    } else {
        fused_network <- step_one[[1]]
    }
    return(fused_network)
}
