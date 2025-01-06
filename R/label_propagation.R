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
#' Given a sol_df derived from training subjects and a full_data_list
#' containing both training and test subjects, re-run SNF to generate a total
#' affinity matrix of both train and subjects and use the label propagation
#' algorithm to assigned predicted clusters to test subjects.
#'
#' @param train_sol_df A solutions data frame derived from the training set. 
#' @param full_dl A data list containing subjects from both the training
#'  and testing sets.
#' @param verbose If TRUE, output progress to console.
#' @return A data frame with one row per observation containing a column for
#'  UIDs, a column for whether the subject was in the train (original) or test
#'  (held out) set, and one column per row of the solutions data frame
#'  indicating the original and propagated clusters.
#' @export
lp_sol_df <- function(train_sol_df,
                      full_dl,
                      verbose = FALSE) {
    ###########################################################################
    # 1. Reorder data list subjects
    ###########################################################################
    if (inherits(train_sol_df, "ext_solutions_df")) {
        train_sol_df <- attributes(train_sol_df)$"solutions_df"
    }
    train_subjects <- uids(train_sol_df)
    all_subjects <- uids(full_dl)
    # Check to make sure the train subjects are all in the full list
    if (!all(train_subjects %in% all_subjects)) {
        metasnf_error(
            "Not all subjects in the provided solutions matrix are present in",
            " the provided data list."
        )
    }
    test_subjects <- all_subjects[!all_subjects %in% train_subjects]
    lp_ordered_subjects <- c(train_subjects, test_subjects)
    full_dl <- reorder_dl_subs(full_dl, lp_ordered_subjects)
    ###########################################################################
    # 2. Prepare vectors containing the names of the train and test subjects
    n_train <- length(train_subjects)
    n_test <- length(test_subjects)
    group_vec <- c(rep("train", n_train), rep("test", n_test))
    ###########################################################################
    sc <- as_snf_config(train_sol_df)
    for (i in seq_len(nrow(train_sol_df))) {
        if (verbose) {
            cat(
                "Processing row ", i, " of ", nrow(train_sol_df), "...\n",
                sep = ""
            )
        }
        current_row <- train_sol_df[i, ]
        sig <- paste0(current_row$"solution")
        #######################################################################
        # The actual SNF
        #######################################################################
        # 1. Run SNF using the full data list
        sdf_row <- sc$"settings_df"[i, ]
        full_fused_network <- snf_step(
            dl = drop_inputs(sdf_row, full_dl),
            scheme = sdf_row$"snf_scheme",
            k = sdf_row$"k",
            alpha = sdf_row$"alpha",
            t = sdf_row$"t",
            cnt_dist_fn = sc$"dist_fns_list"$"cnt_dist_fns"[[sdf_row$"cnt_dist"]],
            dsc_dist_fn = sc$"dist_fns_list"$"dsc_dist_fns"[[sdf_row$"dsc_dist"]],
            ord_dist_fn = sc$"dist_fns_list"$"ord_dist_fns"[[sdf_row$"ord_dist"]],
            cat_dist_fn = sc$"dist_fns_list"$"cat_dist_fns"[[sdf_row$"cat_dist"]],
            mix_dist_fn = sc$"dist_fns_list"$"mix_dist_fns"[[sdf_row$"mix_dist"]],
            weights_row = sc$"weights_matrix"[i, , drop = FALSE]
        )
        full_fused_network <- full_fused_network[
            lp_ordered_subjects,
            lp_ordered_subjects
        ]
        clusters <- t(train_sol_df[i, ])[, 2]
        #######################################################################
        # Label propagation
        #######################################################################
        propagated_labels <- label_prop(full_fused_network, clusters)
        if (i == 1) {
            labeled_df <- data.frame(
                uid = c(train_subjects, test_subjects),
                group = group_vec,
                cluster = propagated_labels
            )
            names <- colnames(labeled_df)
            names[which(names == "cluster")] <- sig
            colnames(labeled_df) <- names
        } else {
            current_df <- data.frame(
                uid = c(train_subjects, test_subjects),
                group = group_vec,
                cluster = propagated_labels
            )
            names <- colnames(current_df)
            names[which(names == "cluster")] <- sig
            colnames(current_df) <- names
            labeled_df <- dplyr::inner_join(
                labeled_df,
                current_df,
                by = c("uid", "group")
            )
        }
    }
    return(labeled_df)
}
