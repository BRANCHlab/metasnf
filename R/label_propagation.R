#' Label propagation
#'
#' Given a full fused network (one containing both pre-labeled training subjects
#'  and unlabeled test-subjects) and the clusters of the pre-labeled subjects,
#'  return a label propagated list of clusters for all subjects.
#'
#' @param full_fused_network Network made by running SNF on training and test
#'  subjects together
#' @param clusters a vector of training subject assigned clusters in matching
#'  order as they appear in full_fused_network
#'
#' @return new_clusters list of cluster labels for all subjects
#'
#' @export
label_prop <- function(full_fused_network, clusters) {
    num_subjects <- nrow(full_fused_network)
    # The y0 matrix stores which cluster everybody belongs to. It is one-hot
    #  encoded. Here it is initialized.
    y0 <- matrix(0, num_subjects, max(clusters))
    # Next, we assign the clusters we already know to be true.
    for (i in seq_along(clusters)){
        y0[i, clusters[i]] <- 1
    }
    p <- full_fused_network / rowSums(full_fused_network)
    nlabel <- which(rowSums(y0) == 0)[1] - 1
    y <- y0
    for (i in 1:1000){
        y <- p %*% y
        y[1:nlabel, ] <- y0[1:nlabel, ]
    }
    new_clusters <- rep(0, num_subjects)
    for (i in seq_len(nrow(y))){
        new_clusters[i] <- which(y[i, ] == max(y[i, ]))
    }
    return(new_clusters)
}


#' Label propagation over an output matrix
#'
#' Given an output matrix and a data_list object containing all subjects, return
#'  a dataframe of the label propagated results of all om rows
#'
#' @param om An output matrix
#' @param full_data_list A data_list object made by rbinding(train, test) data
#'
#' @return labeled_df A dataframe of the label propagated results of all om rows
#'
#' @export
lp_om <- function(om, full_data_list) {
    # Keep a track of the number of train and test subjects
    n_train <- length(colnames(subs(om))) - 1
    n_test <- sdl(full_data_list)$length[1] - n_train
    train_indices <- 1:n_train
    test_indices <- (1 + n_train):(n_test + n_train)
    train_subs <- full_data_list[[1]]$"data"$"subjectkey"[train_indices]
    test_subs <- full_data_list[[1]]$"data"$"subjectkey"[test_indices]
    all_subs <- full_data_list[[1]]$"data"$"subjectkey"
    ordered_subs <- c(train_subs, test_subs)
    group_vec <- c(rep("train", n_train), rep("test", n_test))
    for (i in seq_len(nrow(om))) {
        print(paste0("Processing row ", i, " of ", nrow(om), "."))
        current_row <- om[i, ]
        reduced_dl <- execute_inclusion(full_data_list, current_row)
        check_subj_orders_for_lp(reduced_dl,
                                 current_row,
                                 n_train = n_train,
                                 n_test = n_test)
        full_fused_network <- snf_step(reduced_dl,
                 scheme = current_row$"snf_scheme",
                 K = current_row$"K",
                 alpha = current_row$"alpha"
        )
        full_fused_network <- full_fused_network[ordered_subs, ordered_subs]
        clusters <- get_clusters(current_row)
        propagated_labels <- label_prop(full_fused_network, clusters)
        if (i == 1) {
            labeled_df <- data.frame(
                subjectkey = all_subs,
                group = group_vec,
                cluster = propagated_labels)
            names <- colnames(labeled_df)
            sig <- current_row$"significance"
            names[which(names == "cluster")] <- sig
            colnames(labeled_df) <- names
        } else {
            current_df <- data.frame(
                subjectkey = all_subs,
                group = group_vec,
                cluster = propagated_labels)
            names <- colnames(current_df)
            sig <- current_row$"significance"
            names[which(names == "cluster")] <- sig
            colnames(current_df) <- names
            labeled_df <-
                dplyr::inner_join(labeled_df,
                                  current_df,
                                  by = c("subjectkey", "group"))
        }
    }
    return(labeled_df)
}