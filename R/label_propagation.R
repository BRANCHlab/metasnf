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
    n_test <- summarize_dl(full_data_list)$length[1] - n_train
    train_indices <- 1:n_train
    test_indices <- (1 + n_train):(n_test + n_train)
    # Subject keys of subjects
    train_subs <- full_data_list[[1]]$"data"$"subjectkey"[train_indices]
    test_subs <- full_data_list[[1]]$"data"$"subjectkey"[test_indices]
    all_subs <- full_data_list[[1]]$"data"$"subjectkey"
    ordered_subs <- c(train_subs, test_subs)
    group_vec <- c(rep("train", n_train), rep("test", n_test))
    for (i in seq_len(nrow(om))) {
        print(paste0("Processing row ", i, " of ", nrow(om), "..."))
        current_row <- om[i, ]
        sig <- paste0(current_row$"significance")
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
        if (i == 1) { # if this is the first row of the OM, establish the dataframe
            labeled_df <- data.frame(
                subjectkey = all_subs,
                group = group_vec,
                cluster = propagated_labels)
            names <- colnames(labeled_df)
            names[which(names == "cluster")] <- sig
            colnames(labeled_df) <- names
        } else {
            current_df <- data.frame(
                subjectkey = all_subs,
                group = group_vec,
                cluster = propagated_labels)
            names <- colnames(current_df)
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
