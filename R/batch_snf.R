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
            solutions_matrix <- parallel_batch_snf(
                data_list = data_list,
                settings_matrix = settings_matrix,
                processes = available_cores
            )
            return(solutions_matrix)
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
            solutions_matrix <- parallel_batch_snf(
                data_list = data_list,
                settings_matrix = settings_matrix,
                processes = processes
            )
            return(solutions_matrix)
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
    #  - k: value of affinity matrix hyperparameter
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
        settings_matrix_row <- settings_matrix[i, ]
        current_data_list <- drop_inputs(settings_matrix_row, data_list)
        # Apply the current row's SNF scheme
        current_snf_scheme <- dplyr::case_when(
            settings_matrix_row$"snf_scheme" == 1 ~ "individual",
            settings_matrix_row$"snf_scheme" == 2 ~ "domain",
            settings_matrix_row$"snf_scheme" == 3 ~ "twostep",
        )
        k <- settings_matrix[i, "k"]
        alpha <- settings_matrix[i, "alpha"]
        # 4. Run SNF
        fused_network <- snf_step(
            current_data_list,
            current_snf_scheme,
            k = k,
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
        # If the user has not provided their own list of clustering algorithms,
        #  use the default ones (spectral clustering with eigen-gap or
        #  rotation cost heuristics).
        if (is.null(clust_algs_list)) {
            clust_algs_list <- generate_clust_algs_list()
        }
        # clust_alg stores the function to be used for this run of SNF
        clust_alg <- clust_algs_list[[settings_matrix_row$"clust_alg"]]
        # cluster_results is a named list containing the cluster solution
        #  (vector of which cluster each patient was assigned to) and the
        #  number of clusters for that solution
        cluster_results <- clust_alg(fused_network)
        solution <- cluster_results$"solution"
        nclust <- cluster_results$"nclust"
        # Update the solutions_matrix with the cluster solution and the number
        #  of clusters for that solution
        solutions_matrix[i, rownames(fused_network)] <- solution
        solutions_matrix[i, "nclust"] <- nclust
        #######################################################################
        # 8. Print estimated time taken until function completion
        #######################################################################
        remaining_seconds_vector <- batch_snf_time_remaining(
            seconds_per_row = as.numeric(Sys.time() - start_time),
            rows_remaining = nrow(settings_matrix) - i,
            row = i,
            remaining_seconds_vector
        )
    }
    ###########################################################################
    # 9. Format the final solutions_matrix to be numeric where possible
    ###########################################################################
    solutions_matrix <- numcol_to_numeric(solutions_matrix)
    ###########################################################################
    # 10. Print total time taken for function completion
    ###########################################################################
    total_time <- (proc.time() - start)[["elapsed"]]
    print(paste0("Total time taken: ", round(total_time, 0), " seconds."))
    ###########################################################################
    # 11. Return final solutions_matrix
    ###########################################################################
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
        settings_matrix_row_fn,
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
#' @param settings_matrix_row a row of a settings matrix
#' @param dl a data list
#'
#' @return solutions_matrix_row the corresponding solutions_matrix row
#'
#' @export
settings_matrix_row_fn <- function(settings_matrix_row, dl) {
    settings_matrix_row <- data.frame(t(settings_matrix_row))
    current_data_list <- drop_inputs(settings_matrix_row, dl)
    current_snf_scheme <- dplyr::case_when(
        settings_matrix_row$"snf_scheme" == 1 ~ "individual",
        settings_matrix_row$"snf_scheme" == 2 ~ "domain",
        settings_matrix_row$"snf_scheme" == 3 ~ "twostep",
    )
    k <- settings_matrix_row$"k"
    alpha <- settings_matrix_row$"alpha"
    fused_network <- snf_step(
        current_data_list,
        current_snf_scheme,
        k = k,
        alpha = alpha)
    all_clust <- SNFtool::estimateNumberOfClustersGivenGraph(fused_network)
    # Apply the current row's number of clusters heuristic
    if (settings_matrix_row$"eigen_or_rot" == 1) {
        eigen_best <- all_clust$`Eigen-gap best`
        nclust <- eigen_best
    } else if (settings_matrix_row$"eigen_or_rot" == 2) {
        rot_best <- all_clust$`Rotation cost best`
        nclust <- rot_best
    }
    settings_matrix_row$"nclust" <- nclust
    cluster_results <- SNFtool::spectralClustering(fused_network, nclust)
    # Assign subtype membership
    settings_matrix_row[1, rownames(fused_network)] <- cluster_results
    return(settings_matrix_row)
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
drop_inputs <- function(settings_matrix, data_list) {
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



