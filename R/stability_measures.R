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
    return(subsample_solutions)
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
