#' Label propagation
#'
#' Given a full fused network (one containing both pre-labeled training subjects
#'  and unlabeled test-subjects) and the clusters of the pre-labeled subjects,
#'  return a label propagated list of clusters for all subjects. This function
#'  is derived from SNFtool::groupPredict. Modifications are made to take a
#'  full fused network as input, rather than taking input dataframes and
#'  running SNF internally. This ensures that alternative approaches to
#'  data normalization and distance matrix calculations can be chosen by the
#'  user.
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

#' Label propagation over an solutions matrix
#'
#' Given an solutions matrix and a data_list object containing all subjects, return
#'  a dataframe of the label propagated results of all om rows
#'
#' @param solutions_matrix A solutions matrix
#' @param full_data_list A data_list object made by rbinding(train, test) data
#' @param clust_algs_list List of custom clustering algorithms to apply
#'  to the final fused network. See ?generate_clust_algs_list
#' @param distance_metrics_list A distance_metrics_list.
#'  See ?generate_distance_metrics_list.
#' @param weights_matrix A matrix containing variable weights to use during
#'  distance matrix calculation. See ?generate_weights_matrix.
#'
#' @return labeled_df A dataframe of the label propagated results of all om rows
#'
#' @export
lp_row <- function(solutions_matrix,
                   full_data_list,
                   clust_algs_list = NULL,
                   distance_metrics_list = NULL,
                   weights_matrix = NULL) {
    if (!"significance" %in% colnames(solutions_matrix)) {
        print(paste0(
            "If you add a 'significance' column to your solutions matrix",
            " those values will be used to name each solution (instead of",
            " row IDs)"
        ))
        solutions_matrix$"significance" <- solutions_matrix$"row_id"
    }
    # Keep a track of the number of train and test subjects
    n_train <- length(colnames(subs(solutions_matrix))) - 1
    n_test <- summarize_dl(full_data_list)$length[1] - n_train
    train_indices <- 1:n_train
    test_indices <- (1 + n_train):(n_test + n_train)
    # Subject keys of subjects
    train_subs <- full_data_list[[1]]$"data"$"subjectkey"[train_indices]
    test_subs <- full_data_list[[1]]$"data"$"subjectkey"[test_indices]
    all_subs <- full_data_list[[1]]$"data"$"subjectkey"
    ordered_subs <- c(train_subs, test_subs)
    group_vec <- c(rep("train", n_train), rep("test", n_test))
    #######################################################################
    # 7. Creation of distance_metrics_list, if it does not already exist
    #######################################################################
    if (is.null(distance_metrics_list)) {
        distance_metrics_list <- generate_distance_metrics_list()
    }
    ###########################################################################
    # 8. Create (or check) weights_matrix
    ###########################################################################
    if (is.null(weights_matrix)) {
        weights_matrix <- generate_weights_matrix(
            full_data_list,
            nrow = nrow(solutions_matrix)
        )
    } else {
        if (nrow(weights_matrix) != nrow(solutions_matrix)) {
            stop(
                paste0(
                    "weights_matrix and solutions_matrix should have the same",
                    " number of rows."
                )
            )
        }
    }
    for (i in seq_len(nrow(solutions_matrix))) {
        print(paste0("Processing row ", i, " of ", nrow(solutions_matrix), "..."))
        current_row <- solutions_matrix[i, ]
        sig <- paste0(current_row$"significance")
        reduced_dl <- drop_inputs(current_row, full_data_list)
        check_subj_orders_for_lp(
            reduced_dl,
            current_row,
            n_train = n_train,
            n_test = n_test
        )
        #######################################################################
        # obtaining settings values for snf_step
        scheme <- current_row$"snf_scheme"
        k <- current_row$"k"
        alpha <- current_row$"alpha"
        t <- current_row$"t"
        cont_dist <- current_row$"cont_dist"
        disc_dist <- current_row$"disc_dist"
        ord_dist <- current_row$"ord_dist"
        cat_dist <- current_row$"cat_dist"
        mix_dist <- current_row$"mix_dist"
        cont_dist_fn <- distance_metrics_list$"continuous_distance"[[cont_dist]]
        disc_dist_fn <- distance_metrics_list$"discrete_distance"[[disc_dist]]
        ord_dist_fn <- distance_metrics_list$"ordinal_distance"[[ord_dist]]
        cat_dist_fn <- distance_metrics_list$"categorical_distance"[[cat_dist]]
        mix_dist_fn <- distance_metrics_list$"mixed_distance"[[mix_dist]]
        weights_row <- weights_matrix[i, , drop = FALSE]
        #######################################################################
        full_fused_network <- snf_step(
            reduced_dl,
            scheme = scheme,
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
        full_fused_network <- full_fused_network[ordered_subs, ordered_subs]
        clusters <- get_clusters(current_row)
        propagated_labels <- label_prop(full_fused_network, clusters)
        if (i == 1) { # if this is the first row of the OM, establish the dataframe
            labeled_df <- data.frame(
                subjectkey = all_subs,
                group = group_vec,
                cluster = propagated_labels
            )
            names <- colnames(labeled_df)
            names[which(names == "cluster")] <- sig
            colnames(labeled_df) <- names
        } else {
            current_df <- data.frame(
                subjectkey = all_subs,
                group = group_vec,
                cluster = propagated_labels
            )
            names <- colnames(current_df)
            names[which(names == "cluster")] <- sig
            colnames(current_df) <- names
            labeled_df <- dplyr::inner_join(
                labeled_df,
                current_df,
                by = c("subjectkey", "group")
            )
        }
    }
    return(labeled_df)
}
