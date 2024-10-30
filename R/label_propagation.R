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

#' Label propagate cluster solutions to unclustered subjects
#'
#' Given a solutions_matrix derived from training subjects and a full_data_list
#' containing both training and test subjects, re-run SNF to generate a total
#' affinity matrix of both train and subjects and use the label propagation
#' algorithm to assigned predicted clusters to test subjects.
#'
#' @param train_solutions_matrix A solutions_matrix derived from the training
#' set. The propagation algorithm is slow and should be used for validating a
#' top or top few meaningful chosen clustering solutions. It is advisable to
#' use only a small subset of rows from the original training solutions_matrix
#' for label propagation.
#'
#' @param full_data_list A data_list containing subjects from both the training
#' and testing sets.
#'
#' @param distance_metrics_list Like above - the distance_metrics_list (if any)
#' that was used for the original batch_snf call.
#'
#' @param weights_matrix Like above.
#'
#' @param verbose If TRUE, print progress to console.
#'
#' @return labeled_df a dataframe containing a column for subjectkeys,
#' a column for whether the subject was in the train (original) or test (held
#' out) set, and one column per row of the solutions matrix indicating the
#' original and propagated clusters.
#'
#' @export
lp_solutions_matrix <- function(train_solutions_matrix,
                                full_data_list,
                                distance_metrics_list = NULL,
                                weights_matrix = NULL,
                                verbose = FALSE) {
    ###########################################################################
    # 1. Reorder data_list subjects
    ###########################################################################
    train_subjects <- colnames(subs(train_solutions_matrix))[-1]
    all_subjects <- full_data_list[[1]][[1]]$"subjectkey"
    # Quick check to make sure the train subjects are all in the full list
    if (!all(train_subjects %in% all_subjects)) {
        stop(
            "\n\nAt least one subject from the `train_solutions_matrix`",
            "argument is missing from the `full_data_list` argument.",
            " Please ensure that all subjects in the given solutions matrix",
            " are present in your full data list. E.g., ensure that:",
            "\n\nall(get_cluster_df(train_solutions_matrix)$\"subjectkey\"",
            " %in% collapse_dl(full_data_list)$\"subjectkey\")"
        )
    }
    test_subjects <- all_subjects[!all_subjects %in% train_subjects]
    lp_ordered_subjects <- c(train_subjects, test_subjects)
    full_data_list <- reorder_dl_subs(full_data_list, lp_ordered_subjects)
    ###########################################################################
    # 2. Prepare vectors containing the names of the train and test subjects
    ###########################################################################
    n_train <- length(train_subjects)
    n_test <- length(test_subjects)
    group_vec <- c(rep("train", n_train), rep("test", n_test))
    ###########################################################################
    # 3. SNF of the full data list
    ###########################################################################
    ###########################################################################
    ## 3-1. Creation of distance_metrics_list, if it does not already exist
    ###########################################################################
    if (is.null(distance_metrics_list)) {
        distance_metrics_list <- generate_distance_metrics_list()
    }
    ###########################################################################
    ## 3-2. Create (or check) weights_matrix
    ###########################################################################
    if (is.null(weights_matrix)) {
        weights_matrix <- generate_weights_matrix(
            full_data_list,
            nrow = nrow(train_solutions_matrix)
        )
    } else {
        if (nrow(weights_matrix) != nrow(train_solutions_matrix)) {
            stop(
                paste0(
                    "Weights_matrix and train_solutions_matrix",
                    " should have the same number of rows."
                )
            )
        }
    }
    ###########################################################################
    ## 3-3. SNF one row at a time
    ###########################################################################
    for (i in seq_len(nrow(train_solutions_matrix))) {
        if (verbose) {
            print(
                paste0(
                    "Processing row ", i, " of ",
                    nrow(train_solutions_matrix), "..."
                )
            )
        }
        current_row <- train_solutions_matrix[i, ]
        sig <- paste0(current_row$"row_id")
        reduced_dl <- drop_inputs(current_row, full_data_list)
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
        # The actual SNF
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
        full_fused_network <- full_fused_network[
            lp_ordered_subjects,
            lp_ordered_subjects
        ]
        clusters <- get_clusters(current_row)
        #######################################################################
        # Label propagation
        #######################################################################
        propagated_labels <- label_prop(full_fused_network, clusters)
        if (i == 1) {
            labeled_df <- data.frame(
                subjectkey = c(train_subjects, test_subjects),
                group = group_vec,
                cluster = propagated_labels
            )
            names <- colnames(labeled_df)
            names[which(names == "cluster")] <- sig
            colnames(labeled_df) <- names
        } else {
            current_df <- data.frame(
                subjectkey = c(train_subjects, test_subjects),
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
