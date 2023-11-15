#' Calculate patient co-clustering across subsamples
#'
#' Given a full data_list, subsamples of that data_list, and a row of a
#' settings matrix that outlines how the data should be clustered, calculates
#' across all subsamples the number of times that any pair of patients were in
#' the same subsample as well as the number of times they were in the same
#' cluster. These two pieces of information are stored as matrices that can
#' be supplied to the cocluster_heatmap and pooled_cocluster_heatmap functions.
#'
#' @param data_list A data_list (see ?generate_data_list).
#' @param data_list_subsamples A list of subsampled forms of the data_list
#' (see ?subsample_data_list).
#' @param settings_matrix_row A single row of the settings matrix to calculate
#' co-clustering data for.
#'
#' @return cocluster_data A named list containing two matrices:
#' * "same_solution": A patient x patient matrix where each cell is the number
#'   of subsamples that contained both of those patients
#' * "same_cluster": A patient x patient matrix where each cell is the number
#'   of subsamples where those patients were clustered together
#'
#' @export
generate_cocluster_data <- function(data_list,
                                    data_list_subsamples,
                                    settings_matrix_row) {
    ###########################################################################
    # A list of all the cluster solutions obtained from each subsample
    ###########################################################################
    subsample_solutions <- lapply(
        1:length(data_list_subsamples),
        function(x) {
            solutions_matrix <- batch_snf(
                data_list = data_list_subsamples[[x]],
                settings_matrix_row,
                quiet = TRUE
            )
            cluster_solution <- get_cluster_solutions(solutions_matrix)
            colnames(cluster_solution) <- c("subjectkey", "cluster")
            return(cluster_solution)
        }
    )
    ###########################################################################
    # Generate main matrix storing cocluster information
    ###########################################################################
    subjects <- data_list[[1]]$"data"$"subjectkey"
    same_solution <- matrix(0, length(subjects), length(subjects))
    colnames(same_solution) <- subjects
    rownames(same_solution) <- subjects
    same_cluster <- same_solution # another empty matrix of the same size
    ###########################################################################
    # Loop through all subject pairs and update the matrices
    ###########################################################################
    subject_pairs <- utils::combn(subjects, 2) # each column is a pair
    #for (col in seq_len(ncol(subject_pairs))) {
    #    sub1 <- subject_pairs[1, col]
    #    sub2 <- subject_pairs[2, col]
    #    for (s in subsample_solutions) {
    #        sub1_cluster <- s[s$"subjectkey" == sub1, "cluster"]
    #        sub2_cluster <- s[s$"subjectkey" == sub2, "cluster"]
    #        both_subs_in_s <- length(sub1_cluster) + length(sub2_cluster) == 2
    #        # If both subjects are in this subsample, add to their same
    #        # solution count.
    #        if (both_subs_in_s) {
    #            same_solution[sub1, sub2] <- same_solution[sub1, sub2] + 1
    #            same_solution[sub2, sub1] <- same_solution[sub2, sub1] + 1
    #            # If both subjects have the same cluster in this subsample, add
    #            # to their same cluster count.
    #            if (sub1_cluster == sub2_cluster) {
    #                same_cluster[sub1, sub2] <- same_cluster[sub1, sub2] + 1
    #                same_cluster[sub2, sub1] <- same_cluster[sub2, sub1] + 1
    #            }
    #        }
    #    }
    #}
    subsample_index <- 0
    for (s in subsample_solutions) {
        subsample_index <- subsample_index + 1
        print(
            paste0(
                "Processing subsample ", subsample_index, "/",
                length(subsample_solutions), " ..."
            )
        )
        for (col in seq_len(ncol(subject_pairs))) {
            sub1 <- subject_pairs[1, col]
            sub2 <- subject_pairs[2, col]
            sub1_cluster <- s[s$"subjectkey" == sub1, "cluster"]
            sub2_cluster <- s[s$"subjectkey" == sub2, "cluster"]
            both_subs_in_s <- length(sub1_cluster) + length(sub2_cluster) == 2
            # If both subjects are in this subsample, add to their same
            # solution count.
            if (both_subs_in_s) {
                same_solution[sub1, sub2] <- same_solution[sub1, sub2] + 1
                same_solution[sub2, sub1] <- same_solution[sub2, sub1] + 1
                # If both subjects have the same cluster in this subsample, add
                # to their same cluster count.
                if (sub1_cluster == sub2_cluster) {
                    same_cluster[sub1, sub2] <- same_cluster[sub1, sub2] + 1
                    same_cluster[sub2, sub1] <- same_cluster[sub2, sub1] + 1
                }
            }
        }
    }
    diag(same_solution) <- 1
    diag(same_cluster) <- 1
    cocluster_data <- list(
        "same_solution" = same_solution,
        "same_cluster" = same_cluster
    )
    if (min(same_solution) == 0) {
        pairs <- sum(same_solution == 0)
        warning(
            " There were: ", pairs, " pairs of subjects who were never in",
            " the same resampled data set. To avoid this warning, use a",
            " higher value of subsample_fraction or a higher value of",
            " n_subsamples when calling subsample_data_list."
        )
    }
    return(cocluster_data)
}

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

#' Plot a co-clustering heatmap across multiple settings_matrix rows
#'
#' Generate a heatmap using data from multiple cocluster_data objects. See
#' ?generate_cocluster_data for more information. The resulting heatmap shows
#' how often two patients clustered together off of several resamplings of the
#' data.
#'
#' @param cocluster_list A list of cocluster_data objects (from
#' ?generate_cocluster_data) to pool together for the heatmap.
#'
#' @export
pooled_cocluster_heatmap <- function(cocluster_list) {
    cosolution <- NULL
    cocluster <- NULL
    for (cocluster_data in cocluster_list) {
        if (is.null(cosolution)) {
            cosolution <- cocluster_data$"same_solution"
            cocluster <- cocluster_data$"same_cluster"
        } else {
            if (min(cocluster_data$"same_solution") == 0) {
                stop(
                    "This plot can only be generated if all subject pairs",
                    "have been a part of the same solution at least one time.",
                    "Please increase the number of subsamples or subsample",
                    "fraction used when using the subsample_data_list",
                    "function."
                )
            }
            cosolution <- cosolution + cocluster_data$"same_solution"
            cocluster <- cocluster + cocluster_data$"same_cluster"
        }
    }
    cocluster_matrix <- cocluster / cosolution
    heatmap <- ComplexHeatmap::Heatmap(
        matrix = cocluster_matrix,
        show_row_names = FALSE,
        show_column_names = FALSE,
        col = circlize::colorRamp2(
            c(0, 1),
            c("deepskyblue", "red3")
        ),
        heatmap_legend_param = list(title = "Pooled Co-Cluster Fraction")
    )
    return(heatmap)
}
