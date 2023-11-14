generate_cocluster_data <- function(full_data_list,
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
    subjects <- full_data_list[[1]]$"data"$"subjectkey"
    same_solution <- matrix(0, length(subjects), length(subjects))
    colnames(same_solution) <- subjects
    rownames(same_solution) <- subjects
    same_cluster <- same_solution # another empty matrix of the same size
    ###########################################################################
    # Loop through all subject pairs and update the matrices
    ###########################################################################
    subject_pairs <- combn(subjects, 2) # each column is a pair
    for (col in seq_len(ncol(subject_pairs))) {
        sub1 <- subject_pairs[1, col]
        sub2 <- subject_pairs[2, col]
        for (s in subsample_solutions) {
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
    print(same_solution[1:5, 1:5])
    diag(same_solution) <- 1
    diag(same_cluster) <- 1
    cocluster_data <- list(
        "same_solution" = same_solution,
        "same_cluster" = same_cluster
    )
    return(cocluster_data)
}
