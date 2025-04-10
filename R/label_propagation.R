#' Label propagation
#'
#' Given a full fused network (one containing both pre-clustered observations
#'  and to-be-clustered observations) and the clusters of the pre-clustered 
#'  observations, return a label propagated list of clusters for all observations.
#'  This function is derived from SNFtool::groupPredict. Modifications are made
#'  to take a full fused network as input, rather than taking input data frames
#'  and running SNF internally. This ensures that alternative approaches to
#'  data normalization and distance matrix calculations can be chosen by the
#'  user.
#'
#' @keywords internal
#' @param full_fused_network A network made by running SNF on training and test
#'  observations together.
#' @param clusters A vector of assigned clusters for training observations in
#'  matching order as they appear in full_fused_network.
#' @return A list of cluster labels for all observations.
label_prop <- function(full_fused_network, clusters) {
    num_observations <- nrow(full_fused_network)
    # The y0 matrix stores which cluster everybody belongs to. It is one-hot
    #  encoded. Here it is initialized.
    y0 <- matrix(0, num_observations, max(clusters))
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
    new_clusters <- rep(0, num_observations)
    for (i in seq_len(nrow(y))){
        new_clusters[i] <- which(y[i, ] == max(y[i, ]))
    }
    return(new_clusters)
}

#' Label propagate cluster solutions to non-clustered observations
#'
#' Given a solutions data frame containing clustered observations and a
#' data list containing those clustered observations as well as additional
#' to-be-clustered observations, this function will re-run SNF to generate a
#' similarity matrix of all observations and use the label propagation
#' algorithm to assigned predicted clusters to the non-clustered observations.
#'
#' @param partial_sol_df A solutions data frame derived from the training set. 
#' @param full_dl A data list containing observations from both the training
#'  and testing sets.
#' @param verbose If TRUE, output progress to console.
#' @return A data frame with one row per observation containing a column for
#'  UIDs, a column for whether the observation was in the train (original) or test
#'  (held out) set, and one column per row of the solutions data frame
#'  indicating the original and propagated clusters.
#' @export
#' @examples
#' ## Function to identify observations with complete data
#' #uids_with_complete_obs <- get_complete_uids(
#' #    list(subc_v, income, pubertal, anxiety, depress),
#' #    uid = "unique_id"
#' #)
#' #
#' ## Dataframe assigning 80% of observations to train and 20% to test
#' #train_test_split <- train_test_assign(
#' #    train_frac = 0.8,
#' #    uids = uids_with_complete_obs
#' #)
#' #
#' ## Pulling the training and testing observations specifically
#' #train_obs <- train_test_split$"train"
#' #test_obs <- train_test_split$"test"
#' #
#' ## Partition a training set
#' #train_subc_v <- subc_v[subc_v$"unique_id" %in% train_obs, ]
#' #train_income <- income[income$"unique_id" %in% train_obs, ]
#' #train_pubertal <- pubertal[pubertal$"unique_id" %in% train_obs, ]
#' #train_anxiety <- anxiety[anxiety$"unique_id" %in% train_obs, ]
#' #train_depress <- depress[depress$"unique_id" %in% train_obs, ]
#' #
#' ## Partition a test set
#' #test_subc_v <- subc_v[subc_v$"unique_id" %in% test_obs, ]
#' #test_income <- income[income$"unique_id" %in% test_obs, ]
#' #test_pubertal <- pubertal[pubertal$"unique_id" %in% test_obs, ]
#' #test_anxiety <- anxiety[anxiety$"unique_id" %in% test_obs, ]
#' #test_depress <- depress[depress$"unique_id" %in% test_obs, ]
#' #
#' ## Find cluster solutions in the training set
#' #train_dl <- data_list(
#' #    list(train_subc_v, "subc_v", "neuroimaging", "continuous"),
#' #    list(train_income, "household_income", "demographics", "continuous"),
#' #    list(train_pubertal, "pubertal_status", "demographics", "continuous"),
#' #    uid = "unique_id"
#' #)
#' #
#' ## We'll pick a solution that has good separation over our target features
#' #train_target_dl <- data_list(
#' #    list(train_anxiety, "anxiety", "behaviour", "ordinal"),
#' #    list(train_depress, "depressed", "behaviour", "ordinal"),
#' #    uid = "unique_id"
#' #)
#' #
#' #sc <- snf_config(
#' #    train_dl,
#' #    n_solutions = 5,
#' #    min_k = 10,
#' #    max_k = 30
#' #)
#' #
#' #train_sol_df <- batch_snf(
#' #    train_dl,
#' #    sc,
#' #    return_sim_mats = TRUE
#' #)
#' #
#' #ext_sol_df <- extend_solutions(
#' #    train_sol_df,
#' #    train_target_dl
#' #)
#' #
#' ## Determining solution with the lowest minimum p-value
#' #lowest_min_pval <- min(ext_sol_df$"min_pval")
#' #which(ext_sol_df$"min_pval" == lowest_min_pval)
#' #top_row <- ext_sol_df[1, ]
#' #
#' ## Propagate that solution to the observations in the test set
#' ## data list below has both training and testing observations
#' #full_dl <- data_list(
#' #    list(subc_v, "subc_v", "neuroimaging", "continuous"),
#' #    list(income, "household_income", "demographics", "continuous"),
#' #    list(pubertal, "pubertal_status", "demographics", "continuous"),
#' #    uid = "unique_id"
#' #)
#' #
#' ## Use the solutions data frame from the training observations and the data list
#' ## from the training and testing observations to propagate labels to the test observations
#' #propagated_labels <- label_propagate(top_row, full_dl)
#' #
#' #propagated_labels_all <- label_propagate(ext_sol_df, full_dl)
#' #
#' #head(propagated_labels_all)
#' #tail(propagated_labels_all)
label_propagate <- function(partial_sol_df, full_dl, verbose = FALSE) {
    ###########################################################################
    # 1. Reorder data list observations
    ###########################################################################
    clustered_obs <- uids(partial_sol_df)
    full_obs <- uids(full_dl)
    # Check to make sure the clustered obs are all in the full list
    if (!all(clustered_obs %in% full_obs)) {
        metasnf_error(
            "Not all obs in the provided solutions data frame are presen",
            "t in the provided data list."
        )
    }
    unclustered_obs <- full_obs[!full_obs %in% clustered_obs]
    lp_ordered_obs <- c(clustered_obs, unclustered_obs)
    full_dl <- reorder_dl_uids(full_dl, lp_ordered_obs)
    ###########################################################################
    # 2. Prepare vectors containing the names of the clustered and test obs
    n_clustered <- length(clustered_obs)
    n_test <- length(unclustered_obs)
    group_vec <- c(rep("clustered", n_clustered), rep("unclustered", n_test))
    ###########################################################################
    sc <- as_snf_config(partial_sol_df)
    for (i in seq_len(nrow(partial_sol_df))) {
        if (verbose) {
            cat(
                "Processing row ", i, " of ", nrow(partial_sol_df), "...\n",
                sep = ""
            )
        }
        current_row <- partial_sol_df[i, ]
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
        full_fused_network <- full_fused_network[lp_ordered_obs, lp_ordered_obs]
        clusters <- t(partial_sol_df[i, ])[, 2]
        #######################################################################
        # Label propagation
        #######################################################################
        propagated_labels <- label_prop(full_fused_network, clusters)
        if (i == 1) {
            labeled_df <- data.frame(
                uid = c(clustered_obs, unclustered_obs),
                group = group_vec,
                cluster = propagated_labels
            )
            names <- colnames(labeled_df)
            names[which(names == "cluster")] <- sig
            colnames(labeled_df) <- names
        } else {
            current_df <- data.frame(
                uid = c(clustered_obs, unclustered_obs),
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
