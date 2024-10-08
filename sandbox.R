library(metasnf)
library(ggplot2)

data_list <- generate_data_list(
    list(cort_t, "cortical_thickness", "neuroimaging", "continuous"),
    list(cort_sa, "cortical_area", "neuroimaging", "continuous"),
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)

settings_matrix <- generate_settings_matrix(
    data_list,
    nrow = 4,
    max_k = 40,
    seed = 42
)

solutions_matrix <- batch_snf(data_list, settings_matrix)

set.seed(42)
data_list_subsamples_l <- subsample_data_list(
    data_list,
    n_subsamples = 5,
    subsample_fraction = 0.8
)

set.seed(42)
data_list_subsamples_h <- subsample_data_list(
    data_list,
    n_subsamples = 15,
    subsample_fraction = 0.85
)

# batch snf subsamples will perform the actual SNF across subsampled data lists
batch_snf_subsamples <- function(data_list_subsamples,# {{{
                                 settings_matrix,
                                 processes = 1,
                                 return_similarity_matrices = FALSE,
                                 clust_algs_list = NULL,
                                 suppress_clustering = FALSE,
                                 distance_metrics_list = NULL,
                                 weights_matrix = NULL,
                                 automatic_standard_normalize = FALSE,
                                 return_solutions_matrices = FALSE) {
    # Initialize variables that may get added to end result
    solutions_matrix <- NULL
    similarity_matrices <- NULL
    cluster_solutions <- NULL
    ###########################################################################
    # Generate a new cluster_solutions dataframe for every data_list subsample
    ###########################################################################
    subsample_clusters <- list()
    subsample_solutions_matrices <- list()
    subsample_similarity_matrices <- list()
    for (s in seq_along(data_list_subsamples)) {
        print(
            paste0(
                "Clustering subsample ", s, "/", length(data_list_subsamples),
                "..."
            )
        )
        invisible(
            batch_snf_results <- batch_snf(
                data_list = data_list_subsamples[[s]],
                settings_matrix = settings_matrix,
                processes = processes,
                return_similarity_matrices = return_similarity_matrices,
                clust_algs_list = clust_algs_list,
                suppress_clustering = suppress_clustering,
                distance_metrics_list = distance_metrics_list,
                weights_matrix = weights_matrix,
                automatic_standard_normalize = automatic_standard_normalize,
                quiet = TRUE
            )
        )
        if (inherits(batch_snf_results, "list")) {
            solutions_matrix <- batch_snf_results$"solutions_matrix"
            similarity_matrices <- batch_snf_results$"similarity_matrices"
        } else {
            solutions_matrix <- batch_snf_results 
        }
        # Partioning results
        cluster_solutions <- get_cluster_solutions(solutions_matrix)
        # Appending results to list
        index <- length(subsample_clusters) + 1
        subsample_solutions_matrices[[index]] <- solutions_matrix
        subsample_clusters[[index]] <- cluster_solutions
        subsample_similarity_matrices[[index]] <- similarity_matrices
    }
    subsample_ids <- paste0("subsample_", seq_along(data_list_subsamples))
    results <- list(
        "solutions_matrices" = subsample_solutions_matrices,
        "cluster_solutions" = subsample_clusters,
        "similarity_matrices" = subsample_similarity_matrices
    )
    # Remove empty lists
    results <- results[unlist(lapply(results, function(x) length(x) != 0))]
    results <- lapply(
        results,
        function(x) {
            names(x) <- subsample_ids
            return(x)
        }
    )
    return(results)
}# }}}

# subsample pairwise aris will get the aris across all subsamples
#' Calculate pairwise adjusted Rand indices across subsamples of data{{{
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
subsample_pairwise_aris <- function(subsample_solutions, return_raw_aris = FALSE) {
    ###########################################################################
    # If number of subsamples is less than 3, warn that SD can't be calculated
    ###########################################################################
    if (length(subsample_solutions) < 3) {
        warning(
            "Fewer than 3 subsamples have been provided. Standard",
            " deviation of the pairwise ARIs for each settings matrix row",
            " will not be computed."
        )
    }
    ###########################################################################
    # Skeleton to store the mean and sd of ARIs for each solution
    ###########################################################################
    pairwise_ari_df <- data.frame(
        "row" = integer(),
        "mean_ari" = double(),
        "ari_sd" = double()
    )
    # All the pairwise combinations of subsamples
    pairwise_indices <- utils::combn(length(subsample_solutions), 2)
    subsample_ari_mats <- list()
    ###########################################################################
    # Loop over all the rows of the solutions_matrix
    ###########################################################################
    nrows <- ncol(subsample_solutions[[1]]) - 1
    for (row in seq_len(nrows)) {
        subsample_ari_mat <- matrix(
            nrow = length(subsample_solutions),
            ncol = length(subsample_solutions)
        )
        colnames(subsample_ari_mat) <- paste0(
            "subsample_",
            seq_len(length(subsample_solutions))
        )
        rownames(subsample_ari_mat) <- paste0(
            "subsample_",
            seq_len(length(subsample_solutions))
        )
        print(
            paste0(
                "Calculating pairwise ARIs for row ",
                row, "/", nrows, "..."
            )
        )
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
            # keep column 1 (subjectkey) and column 1 + row
            #  (the solution corresponding to that row)
            solution_a <- subsample_a[, c(1, row + 1)]
            solution_b <- subsample_b[, c(1, row + 1)]
            # remove any subjects who weren't a part of both subsamples
            common_df <- dplyr::inner_join(
                solution_a,
                solution_b,
                by = "subjectkey"
            )
            # The first column of common_df contains the subjectkey values. The
            #  2nd and 3rd columns store the two sets of cluster solutions.
            ari <- mclust::adjustedRandIndex(common_df[, 2], common_df[, 3])
            subsample_ari_mat[v1, v2] <- ari
            subsample_ari_mat[v2, v1] <- ari
            row_adjusted_rand_indices <- c(row_adjusted_rand_indices, ari)
        }
        row_df <- data.frame(
            "row" = row,
            "mean_ari" = mean(row_adjusted_rand_indices),
            "ari_sd" = stats::sd(row_adjusted_rand_indices)
        )
        pairwise_ari_df <- rbind(pairwise_ari_df, row_df)
        diag(subsample_ari_mat) <- 1
        if (return_raw_aris) {
            idx <- length(subsample_ari_mats) + 1
            subsample_ari_mats[[idx]] <- subsample_ari_mat
        }
    }
    if (return_raw_aris) {
        names(subsample_ari_mats) <- paste0("row_", seq_len(nrows))
        results <- list(
            "ari_summary" = pairwise_ari_df,
            "raw_aris" = subsample_ari_mats
        )
    } else {
        results <- pairwise_ari_df
    }
    utils::flush.console()
    return(results)
}# }}}

# Function that calculates % co-clustering
#' Average fraction of clustering together{{{
#'
#' Calculate average fraction of times that observations who clusted
#' together in the full solution continued to cluster together in all the
#' subsampled solutions.
#'
#' @param data_list_subsamples A list of subsets of a larger data_list. See
#' ?subsample_data_list to obtain this object.
#'
#' @param settings_matrix A settings_matrix.
#'
#' @param solutions_matrix A solutions_matrix.
#'
#' @return fraction_together_df Dataframe containing the average fraction
#'  of continued clustering together for all supplied solutions
#'
#' @export
calculate_coclustering <- function(data_list_subsamples,
                                   settings_matrix,
                                   subsample_solutions,
                                   solutions_matrix,
                                   keep_raw = FALSE) {
    # Data frame containing the cluster solutions from the full data_list
    full_cluster_solutions <- get_cluster_solutions(solutions_matrix)
    # Data frame that will store summary data
    fraction_together_df <- data.frame(
        "row" = integer(),
        "mean_fraction_together" = double()
    )
    # List that will optionally track raw coclustering data
    cocluster_count_dfs <- list()
    # Looping over all cluster solutions
    solution_indices <- seq_len(nrow(solutions_matrix))
    for (idx in solution_indices) {
        print(
            paste0(
                "Calculating coclustering fraction for solution ",
                idx, "/", max(solution_indices), "..."
            )
        )
        current_solution_df <- full_cluster_solutions[, c(1, idx + 1)]
        colnames(current_solution_df) <- c("subjectkey", "cluster")
        # Just a vector of the assigned clusters
        current_solution <- current_solution_df[, 2]
        # A vector of the unique clusters that exist in this solution
        unique_clusters <- current_solution |>
            unique() |>
            sort()
        # To make use of dplyr functions
        cluster <- ""
        # A list of dataframes, where each dataframe stores all the observations of
        # a specific cluster
        subs_grouped_by_cluster <- unique_clusters |>
            lapply(
                function(cluster_label) {
                    current_solution_df |>
                        dplyr::filter(cluster %in% cluster_label)
                }
            )
        # Strip clusters that only had a single person
        not_solo_cluster <- lapply(
            subs_grouped_by_cluster,
            function(x) {
                nrow(x) != 1
            }
        ) |> unlist()
        subs_grouped_by_cluster <- subs_grouped_by_cluster[not_solo_cluster]
        # In this lapply call, each initial element is a dataframe with two
        # columns: subjectkey and cluster. Each dataframe also only has one
        # unique value in its cluster column. So each dataframe corresponds
        # to the subjects within one specific dataframe, and the length of the
        # list is equal to the total number of clusters in the given solution.
        # The function used within the lapply call returns a new list of
        # dataframes, one per cluster of the overall solution, that has a 
        # row for every pair of subjects within that cluster.
        clustered_pairs_df_list <- subs_grouped_by_cluster |>
            lapply(
                function(cluster_group) {
                    clustered_pairs <- cluster_group$"subjectkey" |>
                        utils::combn(2) |>
                        t() |>
                        data.frame()
                    colnames(clustered_pairs) <- c("subject_1", "subject_2")
                    rownames(clustered_pairs) <- NULL
                    # Keeping track of which cluster each pair came from
                    clustered_pairs$"original_cluster" <- unique(
                        cluster_group$"cluster"
                    )
                    clustered_pairs$"row" <- idx
                    return(clustered_pairs)
                }
            )
        # A single dataframe where each row contains a pair of observations who
        #  were clustered together. The dataframe covers all such pairings.
        clustered_pairs_df <- do.call("rbind", clustered_pairs_df_list)
        # These columns store the information needed to calculate the average
        #  number of times any pair of same-clustered observations belong to the
        #  same cluster in the subsamples
        clustered_pairs_df$"n_same_solution" <- 0
        clustered_pairs_df$"n_same_cluster" <- 0
        # Optionally initialize raw data matrices
        # Iteration through all the clustered pairs
        for (row in seq_len(nrow(clustered_pairs_df))) {
            # Iteration through all the solutions of subsampled data
            for (sub_ind in seq_len(length(subsample_solutions))) {
                subsample <- subsample_solutions[[sub_ind]]
                subsample_subjects <- subsample$"subjectkey"
                # df with subjectkey and only the current cluster solution
                current_subsample_solution <- subsample[, c(1, idx + 1)]
                colnames(current_subsample_solution) <- c("subjectkey", "cluster")
                current_row <- clustered_pairs_df[row, ]
                sub_1 <- current_row$"subject_1"
                sub_2 <- current_row$"subject_2"
                has_sub_1 <- sub_1 %in% subsample_subjects
                has_sub_2 <- sub_2 %in% subsample_subjects
                if (has_sub_1 && has_sub_2) {
                    clustered_pairs_df[row, ]$"n_same_solution" <- clustered_pairs_df[row, ]$"n_same_solution" + 1
                    sub_1_pos <- current_subsample_solution$"subjectkey" == sub_1
                    sub_2_pos <- current_subsample_solution$"subjectkey" == sub_2
                    sub_1_clust <- current_subsample_solution$"cluster"[sub_1_pos]
                    sub_2_clust <- current_subsample_solution$"cluster"[sub_2_pos]
                    if (sub_1_clust == sub_2_clust) {
                        clustered_pairs_df[row, ]$"n_same_cluster" <- clustered_pairs_df[row, ]$"n_same_cluster" + 1
                    }
                }
            }
        }
        # for dplyr
        n_same_solution <- ""
        n_same_cluster <- ""
        if (min(clustered_pairs_df$"n_same_solution") == 0) {
            incomplete_coverage <- sum(clustered_pairs_df$"n_same_solution" == 0)
            warning(
                paste0(
                    "For cluster solution ", idx, ", ", incomplete_coverage,
                    " out of ", nrow(clustered_pairs_df), " originally",
                    " co-clustered pairs of observations did not appear in",
                    " any of the data list subsamples together. Estimates",
                    " of co-clustering quality may be skewed as a result.",
                    " Consider increasing the value of the",
                    " `subsample_fraction` or",
                    " `n_subsamples` arguments when calling",
                    " `subsample_data_list()`."
                )
            )
        }
        clustered_pairs_df <- clustered_pairs_df |>
            dplyr::mutate(
                frac_together = n_same_cluster / n_same_solution
            )
        mean_fraction_together <- clustered_pairs_df$"frac_together" |> mean(na.rm = TRUE)
        fraction_together_df <- rbind(
            fraction_together_df,
            data.frame(
                "row" = idx,
                "mean_fraction_together" = mean_fraction_together
            )
        )
        if (keep_raw) {
            idx <- length(cocluster_count_dfs) + 1
            cocluster_count_dfs[[idx]] <- clustered_pairs_df
        }
    }
    if (keep_raw) {
        names(cocluster_count_dfs) <- paste0(
            "row_",
            seq_len(nrow(solutions_matrix))
        )
        results <- list(
            "cocluster_count_dfs" = cocluster_count_dfs,
            "cocluster_summary" = fraction_together_df
        )
    } else {
        return(fraction_together_df)
    }
}# }}}

# Visualization for coclustering results
cocluster_density <- function(cocluster_counts) {# {{{
    frac_together <- ""
    original_cluster <- ""
    cocluster_counts$"original_cluster" <- factor(
        cocluster_counts$"original_cluster"
    )
    missing <- sum(is.na(cocluster_counts$"frac_together"))
    # Coverage check
    if (missing > 0) {# {{{
        cocluster_counts <- na.omit(cocluster_counts)
        warning(
            paste0(
                missing, " out of ", nrow(cocluster_counts), " pairs of",
                " observations were",
                " never a part of the same subsampled data list. To avoid",
                " this warning, increase the value of the",
                " `subsample_fraction` or",
                " `n_subsamples` arguments when calling",
                " `subsample_data_list()`."
            )
        )
    }# }}}
    dist_plot <- cocluster_counts |>
        ggplot2::ggplot(
            ggplot2::aes(
                x = frac_together,
                colour = original_cluster
            )
        ) +
        ggplot2::labs(
            x = "Co-clustering Fraction",
            y = "Density",
            colour = "Cluster"
        ) +
        ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(scaled))) +
        ggplot2::theme_bw()
    return(dist_plot)
}# }}}

batch_subsample_results_l <- batch_snf_subsamples(
    data_list_subsamples_l,
    settings_matrix,
    return_similarity_matrices = TRUE,
    return_solutions_matrices = TRUE
)

batch_subsample_results_h <- batch_snf_subsamples(
    data_list_subsamples_h,
    settings_matrix,
    return_similarity_matrices = TRUE,
    return_solutions_matrices = TRUE
)

subsample_cluster_solutions_l <- batch_subsample_results_l$"cluster_solutions"
subsample_sol_mats_l <- batch_subsample_results_l$"solutions_matrices"
subsample_sim_mats_l <- batch_subsample_results_l$"similarity_matrices"

subsample_cluster_solutions_h <- batch_subsample_results_h$"cluster_solutions"
subsample_sol_mats_h <- batch_subsample_results_h$"solutions_matrices"
subsample_sim_mats_h <- batch_subsample_results_h$"similarity_matrices"

pairwise_aris_l <- subsample_pairwise_aris(
    subsample_cluster_solutions_l,
    return_raw_aris = TRUE
)

pairwise_aris_h <- subsample_pairwise_aris(
    subsample_cluster_solutions_h,
    return_raw_aris = TRUE
)

ari_summary_l <- pairwise_aris_l$"ari_summary"
raw_aris_l <- pairwise_aris_l$"raw_aris"

ari_summary_h <- pairwise_aris_h$"ari_summary"
raw_aris_h <- pairwise_aris_h$"raw_aris"

# The manual formula converting an ARI matrix to its average reported value
((sum(as.numeric(pairwise_aris_l$"raw_aris"$"row_4")) - 7)/2)/21

# scales linearly with the number of subsamples and quadratically with the number of
# observations.

#coclustering_results_l <- calculate_coclustering(
#    data_list_subsamples_l,
#    settings_matrix,
#    subsample_cluster_solutions_l,
#    solutions_matrix,
#    keep_raw = TRUE
#)
#
#coclustering_results_h <- calculate_coclustering(
#    data_list_subsamples_h,
#    settings_matrix,
#    subsample_cluster_solutions_h,
#    solutions_matrix,
#    keep_raw = TRUE
#)

calc_coc2 <- function(subsample_solutions,
                      solutions_matrix,
                      keep_raw = FALSE) {
    # For dplyr
    cluster <- ""
    sub_1_clust <- ""
    sub_2_clust <- ""
    # Data frame containing the cluster solutions from the full data_list
    full_cluster_solutions <- get_cluster_solutions(solutions_matrix)
    # Data frame that will store summary data
    cocluster_frac_df <- data.frame()
    # List that will optionally track raw coclustering data
    cocluster_dfs <- list()
    # Looping over all cluster solutions
    solution_indices <- seq_len(nrow(solutions_matrix))
    for (idx in solution_indices) {
        # Print current solution for monitoring progress
        print(
            paste0(
                "Calculating coclustering fraction for solution ",
                idx, "/", max(solution_indices), "..."
            )
        )
        current_solution_df <- full_cluster_solutions[, c(1, idx + 1)]
        colnames(current_solution_df) <- c("subjectkey", "cluster")
        # Unique clusters that exist in this solution
        unique_clusters <- sort(unique(current_solution_df$"cluster"))
        # Dataframe storing all pairs of subjects in the full solution
        cocluster_df <- current_solution_df$"subjectkey" |>
            utils::combn(2) |>
            t() |>
            data.frame()
        colnames(cocluster_df) <- c("sub_1", "sub_2")
        cocluster_df <- dplyr::left_join(
            cocluster_df,
            current_solution_df,
            dplyr::join_by(sub_1 == subjectkey)
        )
        cocluster_df <- dplyr::left_join(
            cocluster_df,
            current_solution_df,
            dplyr::join_by(sub_2 == subjectkey)
        )
        colnames(cocluster_df) <- c(
            "sub_1", "sub_2", "sub_1_clust", "sub_2_clust"
        )
        cocluster_df$"same_solution" <- 0
        cocluster_df$"same_cluster" <- 0
        # Optionally initialize raw data matrices
        # Iteration through all the clustered pairs
        for (row in seq_len(nrow(cocluster_df))) {
            print(row)
            # Iteration through all the solutions of subsampled data
            for (sub_ind in seq_len(length(subsample_solutions))) {
                subsample <- subsample_solutions[[sub_ind]]
                subsample_subjects <- subsample$"subjectkey"
                # df with subjectkey and only the current cluster solution
                current_ss <- subsample[, c(1, idx + 1)]
                colnames(current_ss) <- c("subjectkey", "cluster")
                rownames(current_ss) <- current_ss$"subjectkey"
                sub_1 <- cocluster_df[row, "sub_1"]
                sub_2 <- cocluster_df[row, "sub_2"]
                ss_has_sub_1 <- sub_1 %in% current_ss$"subjectkey"
                ss_has_sub_2 <- sub_2 %in% current_ss$"subjectkey"
                if (ss_has_sub_1 & ss_has_sub_2) {
                    cocluster_df[row, "same_solution"] <-
                        cocluster_df[row, "same_solution"] + 1
                    ss_sub_1_c <- current_ss[sub_1, "cluster"]
                    ss_sub_2_c <- current_ss[sub_2, "cluster"]
                    if (ss_sub_1_c == ss_sub_2_c) {
                        cocluster_df[row, "same_cluster"] <-
                            cocluster_df[row, "same_cluster"] + 1
                    }
                }
            }
        }
        incomplete_coverage <- sum(cocluster_df$"same_solution" == 0)
        if (incomplete_coverage > 0) {
            warning(
                paste0(
                    incomplete_coverage,
                    " out of ", nrow(cocluster_df), " originally",
                    " co-clustered pairs of observations did not appear in",
                    " any of the data list subsamples together. Estimates",
                    " of co-clustering quality may be skewed as a result.",
                    " Consider increasing the value of the",
                    " `subsample_fraction` or",
                    " `n_subsamples` arguments when calling",
                    " `subsample_data_list()`."
                )
            )
        }
        original_cocluster_df <- cocluster_df |>
            dplyr::filter(sub_1_clust == sub_2_clust) |>
            dplyr::mutate(cocluster_frac = same_cluster / same_solution)
        avg_cocluster_frac <- mean(
            original_cocluster_df$"cocluster_frac", na.rm = TRUE
        )
        cocluster_frac_df <- rbind(
            cocluster_frac_df,
            data.frame(
                "row" = idx,
                "avg_cocluster_frac" = avg_cocluster_frac
            )
        )
        if (keep_raw) {
            idx <- length(cocluster_dfs) + 1
            cocluster_dfs[[idx]] <- cocluster_df
        }
    }
    if (keep_raw) {
        results <- list(
            "cocluster_df" = cocluster_dfs,
            "cocluster_summary" = cocluster_frac_df
        )
        return(results)
    } else {
        return(cocluster_frac_df)
    }
}

coclustering_results_l <- calc_coc2(
    subsample_cluster_solutions_l,
    solutions_matrix,
    keep_raw = TRUE
)

subsample_cluster_solutions_l

length(subsample_cluster_solutions_l)

length(data_list_subsamples_l)

no_same <- lapply(
    coclustering_results_l$"cocluster_df",
    function(x) {
        dplyr::filter(x, same_solution == 0)
    }
)

lapply(no_same, nrow)

no_same[[1]]

identical(no_same[[1]], no_same[[2]])

z <- collapse_dl(data_list_subsamples_l[[1]])

z



no_same_1$"sub_1" %in% z$"subjectkey"

no_same_1$"sub_2" %in% z$"subjectkey"

"subject_NDAR_INV0J4PYA5F" %in% z$"subjectkey"

# "cocluster_count_dfs" and "cocluster summary"
cocluster_count_dfs_l <- coclustering_results_l$"cocluster_count_dfs"
cocluster_count_dfs_h <- coclustering_results_h$"cocluster_count_dfs"

head(cocluster_count_dfs_h[[1]])

cocluster_density(cocluster_count_dfs_l[[1]])
cocluster_density(cocluster_count_dfs_h[[1]])

names(coclustering_results)

cocluster_count_dfs_h[[1]]$"n_same_solution"


cocluster_heatmap2 <- function(cocluster_counts, solutions_matrix_row) {
    missing_coclustering <- sum(cocluster_counts$"n_same_solution" == 0)
    if (missing_coclustering > 0) {
        stop(
            missing_coclustering, " out of ", nrow(cocluster_counts),
            " originally",
            " co-clustered pairs of observations did not appear in",
            " any of the data list subsamples together. Co-clustering heatmap",
            " generation requires all original co-clustering pairs to occur",
            " in at least 1 subsampled cluster solution. To avoid this error",
            " try increasing the value of the `subsample_fraction` or",
            " `n_subsamples` arguments when calling `subsample_data_list()`."
        )
    }
    cluster <- ""
    if (nrow(solutions_matrix_row) > 1) {
        warning(
            "This function only plots the one row of a solutions_matrix.",
            " Any row after the first will be skipped."
        )
    }
    cluster_solution <- get_cluster_df(solutions_matrix_row[1, ])
    cluster_solution <- dplyr::arrange(cluster_solution, cluster)
    nsubs <- nrow(cluster_solution)
    # Building skeleton matrices to store same-solution and same-cluster
    # tallies
    same_solution_mat <- matrix(ncol = nsubs, nrow = nsubs)
    diag(same_solution_mat) <- 1
    colnames(same_solution_mat) <- cluster_solution$"subjectkey"
    rownames(same_solution_mat) <- cluster_solution$"subjectkey"
    same_cluster_mat <- same_solution_mat
    # Loop through the coclustering dataframe and populate the matrices
    for (i in seq_len(nrow(cocluster_counts))) {
        row <- cocluster_counts[i, ]
        s1 <- row$"subject_1"
        s2 <- row$"subject_2"
        same_solution_mat[s1, s2]  <- row$"n_same_solution"
        same_solution_mat[s2, s1]  <- row$"n_same_solution"
        same_cluster_mat[s1, s2]  <- row$"n_same_cluster"
        same_cluster_mat[s2, s1]  <- row$"n_same_cluster"
    }
    cocluster_mat <- same_cluster_mat / same_solution_mat
    cocluster_mat[is.na(cocluster_mat)] <- 0
    ComplexHeatmap::Heatmap(
        cocluster_mat,
        show_row_names = FALSE,
        show_column_names = FALSE,
        row_split = cluster_solution$"cluster",
        column_split = cluster_solution$"cluster",
        heatmap_legend_param = list(
            color_bar = "continuous",
            title = "Coclustering\nFraction",
            at = c(0, 0.5, 1)
        ),
        col = circlize::colorRamp2(
            c(0, 0.5, 1),
            c("#92C5DE", "white", "#CA0020")
        )
    )
}
cocluster_heatmap2(
    cocluster_count_dfs_h[[1]],
    solutions_matrix[1, ]
)

cocluster_heatmap2(
    cocluster_count_dfs_l[[1]],
    solutions_matrix[1, ]
)

solutions_matrix

#' Heatmap of patient co-clustering across resampled data
#'
#' Uses the output of generate_cocluster_data (see ?generate_cocluster_data)
#' and returns a well-formatted ComplexHeatmap for visualizing clustering
#' structure across resamplings of the data.
#'
#' @param cocluster_data A named list containing two matrices:
#' * "same_solution": A patient x patient matrix where each cell is the number
#'   of subsamples that contained both of those patients
#' * "same_cluster": A patient x patient matrix where each cell is the number
#'   of subsamples where those patients were clustered together
#'
#' @export
cocluster_heatmap <- function(cocluster_data) {
    same_solution <- cocluster_data$"same_solution"
    same_cluster <- cocluster_data$"same_cluster"
    if (min(same_solution) == 0) {
        stop(
            "This plot can only be generated if all subject pairs have been",
            "a part of the same solution at least one time. Please increase",
            "the number of subsamples or subsample fraction used when using",
            "the subsample_data_list function."
        )
    }
    cocluster_matrix <- same_cluster / same_solution
    heatmap <- ComplexHeatmap::Heatmap(
        matrix = cocluster_matrix,
        show_row_names = FALSE,
        show_column_names = FALSE,
        col = circlize::colorRamp2(
            c(0, 1),
            c("deepskyblue", "red3")
        ),
        heatmap_legend_param = list(title = "Co-Cluster Fraction")
    )
    return(heatmap)
}
