#' Extract dataframe of cluster and subject key from solutions matrix row
#'
#' @param om_row Output matrix row
#'
#' @return cluster_df dataframe of cluster and subjectkey
#'
#' @export
get_cluster_df <- function(om_row) {
    cluster_df <-
        subs(om_row) |>
        t() |>
        data.frame()
    cluster_df$id <- rownames(cluster_df)
    rownames(cluster_df) <- NULL
    colnames(cluster_df) <- c("cluster", "subjectkey")
    cluster_df <- cluster_df[2:nrow(cluster_df), ]
    return(cluster_df)
}

#' Extract list of assigned clusters
#'
#' @param om_row Output matrix row
#'
#' @return clusters list of assigned clusters
#'
#' @export
get_clusters <- function(om_row) {
    cluster_df <-
        subs(om_row) |>
        t() |>
        data.frame()
    cluster_df$id <- rownames(cluster_df)
    rownames(cluster_df) <- NULL
    colnames(cluster_df) <- c("cluster", "subjectkey")
    cluster_df <- cluster_df[2:nrow(cluster_df), ]
    clusters <- cluster_df$"cluster"
    return(clusters)
}

#' Select the top solutions matrix rows for each cluster
#'
#' Given an solutions matrix, returns a dataframe containing the row with the
#' lowest mean p-value and lowest min p-value for cluster sizes 2-5
#'
#' @param om an solutions matrix
#'
#' @return top_clusts_df dataframe with top om rows
#'
#' @export
top_om_per_cluster <- function(om) {
    two_clust <- om[om$"nclust" == 2, ]
    three_clust <- om[om$"nclust" == 3, ]
    four_clust <- om[om$"nclust" == 4, ]
    five_clust <- om[om$"nclust" == 5, ]
    two_clust_min <-
        two_clust[two_clust$min_p_val == min(two_clust$min_p_val), ]
    two_clust_mean <-
        two_clust[two_clust$mean_p_val == min(two_clust$mean_p_val), ]
    if (identical(two_clust_min, two_clust_mean)) {
        two_clust_min$"significance" <- "two_clust_min_and_mean"
        top_two_clust <- two_clust_min
    } else {
        two_clust_min$"significance" <- "two_clust_min"
        two_clust_mean$"significance" <- "two_clust_mean"
        top_two_clust <- rbind(two_clust_min, two_clust_mean)
    }
    three_clust_min <-
        three_clust[three_clust$min_p_val == min(three_clust$min_p_val), ]
    three_clust_mean <-
        three_clust[three_clust$mean_p_val == min(three_clust$mean_p_val), ]
    if (identical(three_clust_min, three_clust_mean)) {
        three_clust_min$"significance" <- "three_clust_min_and_mean"
        top_three_clust <- three_clust_min
    } else {
        three_clust_min$"significance" <- "three_clust_min"
        three_clust_mean$"significance" <- "three_clust_mean"
        top_three_clust <- rbind(three_clust_min, three_clust_mean)
    }
    four_clust_min <-
        four_clust[four_clust$min_p_val == min(four_clust$min_p_val), ]
    four_clust_mean <-
        four_clust[four_clust$mean_p_val == min(four_clust$mean_p_val), ]
    if (identical(four_clust_min, four_clust_mean)) {
        four_clust_min$"significance" <- "four_clust_min_and_mean"
        top_four_clust <- four_clust_min
    } else {
        four_clust_min$"significance" <- "four_clust_min"
        four_clust_mean$"significance" <- "four_clust_mean"
        top_four_clust <- rbind(four_clust_min, four_clust_mean)
    }
    five_clust_min <-
        five_clust[five_clust$min_p_val == min(five_clust$min_p_val), ]
    five_clust_mean <-
        five_clust[five_clust$mean_p_val == min(five_clust$mean_p_val), ]
    if (identical(five_clust_min, five_clust_mean)) {
        five_clust_min$"significance" <- "five_clust_min_and_mean"
        top_five_clust <- five_clust_min
    } else {
        five_clust_min$"significance" <- "five_clust_min"
        five_clust_mean$"significance" <- "five_clust_mean"
        top_five_clust <- rbind(five_clust_min, five_clust_mean)
    }
    top_clusts <- list(
        top_two_clust,
        top_three_clust,
        top_four_clust,
        top_five_clust)
    top_clusts_df <- do.call(rbind, top_clusts)
    return(top_clusts_df)
}

#' Check subject orders for label propagation
#'
#' Prior to label propagation, it is essential that the subject orders of the
#'  full fused network and the supplied clustering information are consistent.
#'
#' @param data_list A data list
#' @param om_row An solutions matrix row
#' @param n_train number of training subjects
#' @param n_test number of testing subjects
#'
#' @return all_checks_passed boolean indicating if all subs are in order
#'
#' @export
check_subj_orders_for_lp <- function(data_list, om_row, n_train, n_test) {
    train_indices <- seq_len(n_train)
    test_indices <- seq_len(n_test) + n_train
    # Comparing training subject orders in data list
    for (i in 1:(length(data_list) - 1)) {
        current_check <-
            identical(data_list[[i]]$"data"$"subjectkey"[train_indices],
                  data_list[[i + 1]]$"data"$"subjectkey"[train_indices])
        if (current_check == FALSE) {
            print(paste0("Mismatch found at position ", i))
            return(NULL)
        }
    }
    # Comparing testing subject orders in data list
    for (i in 1:(length(data_list) - 1)) {
        current_check <-
            identical(data_list[[i]]$"data"$"subjectkey"[test_indices],
                  data_list[[i + 1]]$"data"$"subjectkey"[test_indices])
        if (current_check == FALSE) {
            print(paste0("Mismatch found at position ", i))
            return(NULL)
        }
    }
    # Comparing training subjects between data list and solutions matrix...
    current_row_names <- subs(om_row) |>
        dplyr::select(dplyr::starts_with("subject_")) |>
        colnames()
    current_check <-
        identical(data_list[[i]]$"data"$"subjectkey"[train_indices],
                  current_row_names)
    if (current_check == FALSE) {
        stop(paste0(
            "\n",
            "Mismatch found between OM colnames and data list subject order.\n",
            "\nIn order to perform label propagation, the order of subjects in",
            " the full data list must match equal the training order followed",
            " by the testing order. \n\nTo remedy this issue, rebuild your",
            " full_data_list object while specifying the train_subjects",
            " and test_subjects arguments."))
    }
    all_checks_passed <- TRUE
    return(all_checks_passed)
}
