#' Run variations of SNF.
#'
#' This is the core function of the metasnf package. Using the information
#'  stored in a settings_matrix (see ?generate_settings_matrix) and a data_list
#'  (see ?generate_data_list), run repeated complete SNF pipelines to generate
#'  a broad space of post-SNF cluster solutions.
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @param settings_matrix A data.frame where each row completely defines an SNF
#'  pipeline transforming individual input dataframes into a final cluster
#'  solution. See ?generate_settings_matrix or
#'  https://branchlab.github.io/metasnf/articles/settings_matrix.html for more
#'  details.
#'
#' @param processes Specify number of processes used to complete SNF iterations
#'  * `1` (default) Sequential processing: function will iterate through the
#'    `settings_matrix` one row at a time with a for loop. This option will
#'     not make use of multiple CPU cores, but will show a progress bar.
#'  * `2` or higher: Parallel processing will use the
#'    `future.apply::future_apply` to distribute the SNF iterations across
#'    the specified number of CPU cores. If higher than the number of
#'    available cores, a warning will be printed and the maximum number of
#'    cores will be used.
#'  * `max`: All available cores will be used.
#'
#' @param return_similarity_matrices If TRUE, function will return a list where
#'  the first element is the solutions matrix and the second element is a list
#'  of similarity matrices for each row in the solutions_matrix. Default FALSE.
#'
#' @param similarity_matrix_dir If specified, this directory will be used to
#'  save all generated similarity matrices.
#'
#' @param clust_algs_list List of custom clustering algorithms to apply
#'  to the final fused network. See ?generate_clust_algs_list.
#'
#' @param suppress_clustering If FALSE (default), will apply default or custom
#'  clustering algorithms to provide cluster solutions on every iteration of
#'  SNF. If TRUE, parameter `similarity_matrix_dir` must be specified.
#'
#' @param distance_metrics_list An optional nested list containing which
#'  distance metric function should be used for the various variable types
#'  (continuous, discrete, ordinal, categorical, and mixed). See
#'  ?generate_distance_metrics_list for details on how to build this.
#'
#' @param weights_matrix A matrix containing variable weights to use during
#'  distance matrix calculation. See ?generate_weights_matrix for details on
#'  how to build this.
#'
#' @param automatic_standard_normalize If TRUE, will automatically apply
#' standard normalization prior to calculation of any distance matrices. This
#' parameter cannot be used in conjunction with a custom distance metrics list.
#' If you wish to supply custom distance metrics but also always have standard
#' normalization, simply ensure that the numeric (continuous, discrete, and
#' ordinal) distance metrics are only populated with distance metric functions
#' that apply standard normalization. See
#' https://branchlab.github.io/metasnf/articles/distance_metrics.html to learn
#' more.
#'
#' @param quiet If TRUE, the function won't print out time remaining estimates.
#'
#' @return populated_settings_matrix settings matrix with filled columns
#'  related to subtype membership
#'
#' @export
batch_snf <- function(data_list,
                      settings_matrix,
                      processes = 1,
                      return_similarity_matrices = FALSE,
                      similarity_matrix_dir = NULL,
                      clust_algs_list = NULL,
                      suppress_clustering = FALSE,
                      distance_metrics_list = NULL,
                      weights_matrix = NULL,
                      automatic_standard_normalize = FALSE,
                      quiet = FALSE) {
    ###########################################################################
    # 1. Start timer to keep track of entire function duration
    ###########################################################################
    if (!quiet) {
        start <- proc.time() # final time taken for entire function
        remaining_seconds_vector <- vector() # estimate time to completion
    }
    ###########################################################################
    # 2. Ensure settings_matrix is a data.frame (not a tibble or matrix)
    ###########################################################################
    settings_matrix <- data.frame(settings_matrix)
    ###########################################################################
    # 3. Check validity of settings
    ###########################################################################
    # The user may have chosen to simultaneously not save similarity matrices
    # and to not apply any clustering algorithms. In that case, this function
    # is not really doing anything. Stop the function with an error.
    no_similarity_matrices <-
        is.null(similarity_matrix_dir) & !return_similarity_matrices
    if (no_similarity_matrices & suppress_clustering) {
        stop(
            paste0(
               "batch_snf has been called with the suppress_clustering",
               " parameter set to TRUE (no clustering will occur), no path",
               " provided in the similarity_matrix_dir parameter for storing",
               " matrices, and return_similarity_matrices set to FALSE so that",
               " similarity matrices are not being returned. With this",
               " combination of settings, the batch_snf function yields no",
               " meaningful output."
            )
        )
    }
    # If there is a value of the k hyperparameter that exceeds the number
    # of patients in the data_list, SNFtool::affinityMatrix cannot run. This
    # check can't go in the generate_settings_matrix function in case the user
    # creater their base settings_matrix with a valid k, then extended their
    # settings matrix using the add_settings_matrix_rows function with an
    # invalid k (a function that doesn't require users to supply the data).
    max_k <- max(settings_matrix$"k")
    n_patients <- unique(summarize_dl(data_list)$"length")
    # Ensure that the maximum k value doesn't exceed the number of subjects
    if (max_k >= n_patients) {
        stop(
            paste0(
                "The highest value of k in your settings_matrix exceeds the",
                " number of patients in your data. Please provide another",
                " settings_matrix while ensuring the maximum k value",
                " (currently ", max_k, ") is less than the number of patients",
                " in the data: ", n_patients, "."
            )
        )
    }
    ###########################################################################
    # 4. Creation of distance_metrics_list if it does not already exist
    ###########################################################################
    if (is.null(distance_metrics_list)) {
        # Make sure the user didn't just forget to provide their list. If the
        #  settings matrix has distances chosen beyond a value of 1, the user
        #  certainly meant to provide a custom list.
        max_dist_param <- max(
            settings_matrix$"cont_dist",
            settings_matrix$"disc_dist",
            settings_matrix$"ord_dist",
            settings_matrix$"cat_dist",
            settings_matrix$"mix_dist"
        )
        if (max_dist_param > 1) {
            stop(
                "settings_matrix refers to distance metrics values beyond one",
                " but no distance_metrics_list was provided. Did you forget",
                " to provide a distance_metrics_list?"
            )
        } else {
            if (automatic_standard_normalize) {
                # Generate a list where numeric distances are all standard
                # normalized.
                distance_metrics_list <- generate_distance_metrics_list(
                    continuous_distances = list(
                        "sn_euclidean_distance" = sn_euclidean_distance
                    ),
                    discrete_distances = list(
                        "sn_euclidean_distance" = sn_euclidean_distance
                    ),
                    ordinal_distances = list(
                        "sn_euclidean_distance" = sn_euclidean_distance
                    ),
                    categorical_distances = list(
                        "gower_distance" = gower_distance
                    ),
                    mixed_distances = list(
                        "gower_distance" = gower_distance
                    ),
                    keep_defaults = FALSE
                )
            } else {
                distance_metrics_list <- generate_distance_metrics_list()
            }
        }
    } else {
        if (automatic_standard_normalize) {
            stop(
                "The automatic_standard_normalize parameter cannot be used",
                "at the same time as a custom distance metrics list. Please",
                "ensure the custom distance metrics you are providing all",
                "have built in standard normalization."
            )
        }
    }
    ###########################################################################
    # 5. Creation of weights_matrix, if it does not already exist
    ###########################################################################
    if (is.null(weights_matrix)) {
        weights_matrix <- generate_weights_matrix(
            data_list,
            nrow = nrow(settings_matrix)
        )
    } else {
        if (nrow(weights_matrix) != nrow(settings_matrix)) {
            stop(
                paste0(
                    "weights_matrix and settings_matrix should have the same",
                    " number of rows."
                )
            )
        }
    }
    ###########################################################################
    # 6. Creation of clust_algs_list, if it does not already exist
    ###########################################################################
    # If the user has not provided their own list of clustering algorithms,
    #  use the default ones (spectral clustering with eigen-gap or
    #  rotation cost heuristics).
    if (is.null(clust_algs_list)) {
        clust_algs_list <- generate_clust_algs_list()
    }
    ###########################################################################
    # 7. Call separate function for parallel processing
    ###########################################################################
    if (processes != 1) {
        available_cores <- max(future::availableCores())
        # Use all available cores
        if (processes == "max") {
            solutions_matrix <- parallel_batch_snf(
                data_list = data_list,
                distance_metrics_list = distance_metrics_list,
                clust_algs_list = clust_algs_list,
                settings_matrix = settings_matrix,
                weights_matrix = weights_matrix,
                similarity_matrix_dir = similarity_matrix_dir,
                return_similarity_matrices = return_similarity_matrices,
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
                distance_metrics_list = distance_metrics_list,
                clust_algs_list = clust_algs_list,
                settings_matrix = settings_matrix,
                weights_matrix = weights_matrix,
                similarity_matrix_dir = similarity_matrix_dir,
                return_similarity_matrices = return_similarity_matrices,
                processes = processes
            )
            return(solutions_matrix)
        # Invalid input check
        } else {
            stop("Invalid number of processes specified.")
        }
    }
    ###########################################################################
    # 8. Single thread - Create solutions_matrix
    ###########################################################################
    # Creation of solutions_matrix (where clustering results are stored)
    # solutions_matrix is a dataframe with the following columns:
    #  - row_id (1 column): number matching the row of the settings_matrix used
    #    to generate this solution
    #  - inc_* (1 column per input df): Binary indicating if the input df was
    #    incuded (1) or excluded (0) for this row of SNF
    #  - snf_scheme (1 column): number indicating which of the preprogrammed
    #    'schemes' was used to for this run of SNF
    #  - alpha (AKA sigma or eta): value of similarity matrix hyperparameter
    #  - k: value of similarity matrix hyperparameter
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
    if (!suppress_clustering) {
        solutions_matrix <- add_columns(
            df = settings_matrix,
            newcols = data_list[[1]]$"data"$"subjectkey", # one col/patient
            fill = 0 # populate the new column with 0s by default
        )
        # 5b. solutions_matrix gets one new column to store the cluster that
        # each subject was assigned to.
        solutions_matrix <- add_columns(
            df = settings_matrix,
            newcols = "nclust",
            fill = 0
        )
    }
    ###########################################################################
    # 9. Creation of list to store similarity matrices (if requested)
    ###########################################################################
    if (isTRUE(return_similarity_matrices)) {
        similarity_matrices <- list()
    }
    ###########################################################################
    # 10. Iterate through the rows of the settings matrix
    ###########################################################################
    for (i in seq_len(nrow(settings_matrix))) {
        start_time <- Sys.time() # used to estimate time to completion
        settings_matrix_row <- settings_matrix[i, ]
        weights_row <- weights_matrix[i, , drop = FALSE]
        current_data_list <- drop_inputs(settings_matrix_row, data_list)
        # Apply the current row's SNF scheme
        current_snf_scheme <- dplyr::case_when(
            settings_matrix_row$"snf_scheme" == 1 ~ "individual",
            settings_matrix_row$"snf_scheme" == 2 ~ "domain",
            settings_matrix_row$"snf_scheme" == 3 ~ "twostep",
        )
        k <- settings_matrix_row$"k"
        alpha <- settings_matrix_row$"alpha"
        t <- settings_matrix_row$"t"
        cont_dist <- settings_matrix_row$"cont_dist"
        disc_dist <- settings_matrix_row$"disc_dist"
        ord_dist <- settings_matrix_row$"ord_dist"
        cat_dist <- settings_matrix_row$"cat_dist"
        mix_dist <- settings_matrix_row$"mix_dist"
        cont_dist_fn <- distance_metrics_list$"continuous_distance"[[cont_dist]]
        disc_dist_fn <- distance_metrics_list$"discrete_distance"[[disc_dist]]
        ord_dist_fn <- distance_metrics_list$"ordinal_distance"[[ord_dist]]
        cat_dist_fn <- distance_metrics_list$"categorical_distance"[[cat_dist]]
        mix_dist_fn <- distance_metrics_list$"mixed_distance"[[mix_dist]]
        # Run SNF
        fused_network <- snf_step(
            current_data_list,
            current_snf_scheme,
            k = k,
            alpha = alpha,
            t = t,
            cont_dist_fn = cont_dist_fn,
            disc_dist_fn = disc_dist_fn,
            ord_dist_fn = ord_dist_fn,
            cat_dist_fn = cat_dist_fn,
            mix_dist_fn = mix_dist_fn,
            weights_row = weights_row
        )
        # If user provided a path to save the similarity matrices, save them
        if (!is.null(similarity_matrix_dir)) {
            utils::write.csv(
                x = fused_network,
                file = similarity_matrix_path(similarity_matrix_dir, i),
                row.names = TRUE
            )
        }
        # If the user requested all similarity matrices are returned, add to list
        if (isTRUE(return_similarity_matrices)) {
            similarity_matrices[[length(similarity_matrices) + 1]] <- fused_network
        }
        #######################################################################
        # 11. Clustering of the final fused network
        #######################################################################
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
        if (!suppress_clustering) {
            solutions_matrix[i, rownames(fused_network)] <- solution
            solutions_matrix[i, "nclust"] <- nclust
        }
        #######################################################################
        # 12. Print estimated time taken until function completion
        #######################################################################
        if (!quiet) {
            remaining_seconds_vector <- batch_snf_time_remaining(
                seconds_per_row = as.numeric(Sys.time() - start_time),
                rows_remaining = nrow(settings_matrix) - i,
                row = i,
                remaining_seconds_vector
            )
        }
    }
    ###########################################################################
    # 13. Format the final solutions_matrix to be numeric where possible
    ###########################################################################
    if (!suppress_clustering) {
        solutions_matrix <- numcol_to_numeric(solutions_matrix)
    }
    ###########################################################################
    # 14. Print total time taken for function completion
    ###########################################################################
    if (!quiet) {
        total_time <- (proc.time() - start)[["elapsed"]]
        print(paste0("Total time taken: ", round(total_time, 0), " seconds."))
    }
    ###########################################################################
    # 15. Return final output
    ###########################################################################
    # The user requested that similarity matrices are returned. Create a list
    #  containing the solutions matrix as well as the similarity matrices and
    #  return that.
    if (isTRUE(return_similarity_matrices)) {
        check_similarity_matrices(similarity_matrices)
        if (!suppress_clustering) {
            # the user wants similarity matrices and solutions matrix
            batch_snf_results <- list(
                solutions_matrix,
                similarity_matrices
            )
            names(batch_snf_results) <- c(
                "solutions_matrix",
                "similarity_matrices"
            )
            return(batch_snf_results)
        } else {
            # the user wants similarity matrices but no solutions matrix
            return(similarity_matrices)
        }
    } else {
        # The user did not request that similarity matrices are returned. Just
        #  return the solutions matrix. Don't need to check if solutions
        #  matrices are requested - that was handled earlier in the funciton.
        return(solutions_matrix)
    }
}

#' Execute inclusion
#'
#' Given a data list and a settings matrix row, returns a data list of selected
#'  inputs
#'
#' @param settings_matrix matrix indicating parameters to iterate SNF through
#' @param data_list A nested list of input data from `generate_data_list()`.
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
