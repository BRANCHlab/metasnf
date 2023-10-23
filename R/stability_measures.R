#' Create subsamples of a data_list
#'
#' @param data_list A data_list.
#' @param n_subsamples Number of subsamples to create.
#' @param subsample_fraction Percentage of patients to include per subsample.
#' @param n_subjects Number of patients to include per subsample.
#'
#' @return data_list_subsamples A list of subsampled data_list objects.
#'
#' @export
subsample_data_list <- function(data_list,
                                n_subsamples,
                                subsample_fraction = NULL,
                                n_subjects = NULL) {
    # Make sure that only one parameter was used to specify how many subjects
    #  to keep in each subsample
    both_null <- is.null(subsample_fraction) & is.null(n_subjects)
    neither_null <- !is.null(subsample_fraction) & !is.null(n_subjects)
    if (both_null | neither_null) {
        stop(
            paste0(
                "Either the subsample_fraction parameter (fraction of",
                " subjects) or n_subjects (number of subjects) must be",
                " provided. Not both (or neither)."
            )
        )
    }
    # Calculate number of subjects to keep if fraction parameter was used
    all_subjects <- data_list[[1]]$"data"$"subjectkey"
    # Ensure n_subjects is within 0 and the total number of subjects
    if (!is.null(n_subjects)) {
        if (n_subjects < 0 | n_subjects > length(all_subjects)) {
            stop(
                paste0(
                    "n_subjects must be between 0 and the total number of",
                    " subjects."
                )
            )
        } else if (as.integer(n_subjects) != n_subjects) {
            stop(
                "n_subjects must be an integer."
            )
        }
    }
    # Ensure sample fraction is a real fraction
    if (!is.null(subsample_fraction)) {
        if (subsample_fraction > 1 | subsample_fraction < 0) {
            stop(
                "subsample_fraction must be between 0 and 1."
            )
        } else {
            n_subjects <- round(subsample_fraction * length(all_subjects))
        }
    }
    subject_subsamples <- lapply(
        rep(n_subjects, n_subsamples),
        function(x) {
            return(sample(all_subjects, x))
        }
    )
    data_list_subsamples <- subject_subsamples |> lapply(
        function(subsample) {
            length(subsample)
            dl_subsample <- data_list |> lapply(
                function(x) {
                    chosen_rows <- x$"data"$"subjectkey" %in% subsample
                    x$"data" <- x$"data"[chosen_rows, ]
                    return(x)
                }
            )
        }
    )
    subsample_names <- paste0("subsample_", 1:n_subsamples)
    names(data_list_subsamples) <- subsample_names
    return(data_list_subsamples)
}

#' Calculate pairwise adjusted Rand indices across subsamples of data
#'
#' @param data_list_subsamples A list of subsets of a larger data_list. See
#'  ?subsample_data_list to obtain this object.
#' @param settings_matrix A settings_matrix.
#'
#' @return pairwise_ari_df Dataframe containing a column for row (which row of
#'  the original settings_matrix the rest of the information corresponds to),
#'  mean_ari (the average adjusted Rand Index across all subsamples), and
#'  ari_sd (the standard deviation of the adjusted Rand Indices).
#'
#' @export
subsample_pairwise_aris <- function(data_list_subsamples, settings_matrix) {
    # Generate a new cluster_solutions dataframe for every data_list subsample
    pb <- utils::txtProgressBar(
        min = 0,
        max = length(data_list_subsamples),
        style = 3
    )
    subsample_solutions <- lapply(
        1:length(data_list_subsamples),
        function(x) {
            invisible(
                solutions_matrix <- batch_snf(
                    data_list = data_list_subsamples[[x]],
                    settings_matrix,
                    quiet = TRUE
                )
            )
            cluster_solutions <- get_cluster_solutions(solutions_matrix)
            utils::setTxtProgressBar(pb, x)
            return(cluster_solutions)
        }
    )
    # Skeleton to store the mean and sd of ARIs for each solution
    pairwise_ari_df <- data.frame(
        "row" = integer(),
        "mean_ari" = double(),
        "ari_sd" = double()
    )
    # All the pairwise combinations of subsamples
    pairwise_indices <- utils::combn(length(subsample_solutions), 2)
    # Loop over all the rows of the solutions_matrix
    for (solutions_matrix_row in seq_len(ncol(subsample_solutions[[1]]) - 1)) {
        row_adjusted_rand_indices <- vector()
        # Loop over all pairs of subsamples for that row
        for (col in seq_len(ncol(pairwise_indices))) {
            if (col %% 100 == 0) {
                progress <- 100 * col / ncol(pairwise_indices)
                cat("\r", progress, "% completed...", sep = "")
            }
            # v1 and v2 are indices of two distinct subsamples
            v1 <- pairwise_indices[1, col]
            v2 <- pairwise_indices[2, col]
            subsample_a <- subsample_solutions[[v1]]
            subsample_b <- subsample_solutions[[v2]]
            # keep column 1 (subjectkey) and column 1 + solutions_matrix_row
            #  (the solution corresponding to that solutions_matrix_row)
            solution_a <- subsample_a[, c(1, solutions_matrix_row + 1)]
            solution_b <- subsample_b[, c(1, solutions_matrix_row + 1)]
            # remove any subjects who weren't a part of both subsamples
            common_df <- dplyr::inner_join(
                solution_a,
                solution_b,
                by = "subjectkey"
            )
            # The first column of common_df contains the subjectkey values. The
            #  2nd and 3rd columns store the two sets of cluster solutions.
            ari <- mclust::adjustedRandIndex(common_df[, 2], common_df[, 3])
            row_adjusted_rand_indices <- c(row_adjusted_rand_indices, ari)
        }
        row_df <- data.frame(
            "row" = solutions_matrix_row,
            "mean_ari" = mean(row_adjusted_rand_indices),
            "ari_sd" = stats::sd(row_adjusted_rand_indices)
        )
        pairwise_ari_df <- rbind(pairwise_ari_df, row_df)
    }
    cat("\n")
    return(pairwise_ari_df)
}

#' Average fraction of clustering together
#'
#' Calculate average fraction of times that patients who clusted
#'  together in the full solution continued to cluster together in all the
#'  subsampled solutions.
#'
#' @param data_list_subsamples A list of subsets of a larger data_list. See
#'  ?subsample_data_list to obtain this object.
#' @param settings_matrix A settings_matrix.
#' @param solutions_matrix A solutions_matrix.
#'
#' @return fraction_together_df Dataframe containing the average fraction
#'  of continued clustering together for all supplied solutions
#'
#' @export
fraction_clustered_together <- function(data_list_subsamples,
                                        settings_matrix,
                                        solutions_matrix) {
    subsample_solutions <- lapply(
        1:length(data_list_subsamples),
        function(x) {
            solutions_matrix <- batch_snf(
                data_list = data_list_subsamples[[x]],
                settings_matrix,
                quiet = TRUE
            )
            cluster_solutions <- get_cluster_solutions(solutions_matrix)
            return(cluster_solutions)
        }
    )
    # Dataframe containing the cluster solutions from the full data_list
    full_cluster_solutions <- get_cluster_solutions(solutions_matrix)
    # The solution we are interested in
    fraction_together_df <- data.frame(
        "row" = integer(),
        "mean_fraction_together" = double()
    )
    solution_indices <- seq_len(nrow(solutions_matrix))
    pb <- utils::txtProgressBar(
        min = 0,
        max = nrow(solutions_matrix),
        style = 3
    )
    for (solution_index in solution_indices) {
        utils::setTxtProgressBar(pb, solution_index)
        current_solution_df <-
            full_cluster_solutions[, c(1, solution_index + 1)]
        colnames(current_solution_df) <- c("subjectkey", "cluster")
        # Just a vector of the assigned clusters
        current_solution <- current_solution_df[, 2]
        # A vector of the unique clusters that exist in this solution
        unique_clusters <- current_solution |>
            unique() |>
            sort()
        # To make use of dplyr functions
        cluster <- ""
        # A list of dataframes, where each dataframe stores all the patients of
        # a specific cluster
        subs_grouped_by_cluster <- unique_clusters |>
            lapply(
                function(cluster_label) {
                    current_solution_df |>
                        dplyr::filter(cluster %in% cluster_label)
                }
            )
        # A list of dataframes, where each dataframe stores all the pairs of
        #  patients of a specific cluster
        clustered_pairs_df_list <- subs_grouped_by_cluster |>
            lapply(
                function(cluster_group) {
                    clustered_pairs <- cluster_group$"subjectkey" |>
                        utils::combn(2) |>
                        t() |>
                        data.frame()
                    colnames(clustered_pairs) <- c("subject_1", "subject_2")
                    rownames(clustered_pairs) <- NULL
                    return(clustered_pairs)
                }
            )
        # A single dataframe where each row contains a pair of patients who
        #  were clustered together. The dataframe covers all such pairings.
        clustered_pairs_df <- do.call("rbind", clustered_pairs_df_list)
        # These columns store the information needed to calculate the average
        #  number of times any pair of same-clustered patients belong to the
        #  same cluster in the subsamples
        clustered_pairs_df$"n_same_solution" <- 0
        clustered_pairs_df$"n_same_cluster" <- 0
        # Iteration through all the clustered pairs
        for (row in seq_len(nrow(clustered_pairs_df))) {
            # Iteration through all the solutions of subsampled data
            for (sub_ind in seq_len(length(subsample_solutions))) {
                subsample <- subsample_solutions[[sub_ind]]
                subsample_subjects <- subsample$"subjectkey"
                # df with subjectkey and only the current cluster solution
                current_subsample_solution <-
                    subsample[, c(1, solution_index + 1)]
                colnames(current_subsample_solution) <-
                    c("subjectkey", "cluster")
                current_row <- clustered_pairs_df[row, ]
                sub_1 <- current_row$"subject_1"
                sub_2 <- current_row$"subject_2"
                has_sub_1 <- sub_1 %in% subsample_subjects
                has_sub_2 <- sub_2 %in% subsample_subjects
                if (has_sub_1 & has_sub_2) {
                    clustered_pairs_df[row, ]$"n_same_solution" <-
                        clustered_pairs_df[row, ]$"n_same_solution" + 1
                    sub_1_pos <-
                        current_subsample_solution$"subjectkey" == sub_1
                    sub_2_pos <-
                        current_subsample_solution$"subjectkey" == sub_2
                    sub_1_clust <-
                        current_subsample_solution$"cluster"[sub_1_pos]
                    sub_2_clust <-
                        current_subsample_solution$"cluster"[sub_2_pos]
                    if (sub_1_clust == sub_2_clust) {
                        clustered_pairs_df[row, ]$"n_same_cluster" <-
                            clustered_pairs_df[row, ]$"n_same_cluster" + 1
                    }
                }
            }
        }
        # for dplyr
        n_same_solution <- ""
        n_same_cluster <- ""
        clustered_pairs_df <- clustered_pairs_df |>
            dplyr::filter(n_same_solution != 0) |>
            dplyr::mutate(
                frac_together = n_same_cluster / n_same_solution
            )
        mean_fraction_together <- clustered_pairs_df$"frac_together" |> mean()
        fraction_together_df <- rbind(
            fraction_together_df,
            data.frame(
                "row" = solution_index,
                "mean_fraction_together" = mean_fraction_together
            )
        )
    }
    return(fraction_together_df)
}
