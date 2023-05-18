#' Select specific row_ids from an output matrix
#'
#' @param om output matrix
#' @param row_ids vector of row_id values to be selected
#'
#' @return selected_om
#'
#' @export
select_om <- function(om, row_ids) {
    selected_om <- om[om$"row_id" %in% row_ids, ]
    return(selected_om)
}

#' Extract dataframe of cluster and subject key from output matrix row
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

#' Calculate overall p-values for a characterization_df
#'
#' @param characterization_df A merged list containing cluster, subjectkey, and
#'  various CBCL outcomes
#' @param bonferroni boolean for reporting bonferroni corrected p-values
#'
#' @export
cbcl_ord_reg <- function(characterization_df, bonferroni = FALSE) {
    outcomes <- characterization_df |>
        dplyr::select(dplyr::starts_with("cbcl")) |>
        colnames()
    ord_reg_df <- data.frame(
        outcome = as.character(),
        pval = as.numeric())
    for (outcome in outcomes) {
        outcome_df <- characterization_df[, c("subjectkey", outcome)]
        cluster_df <- characterization_df[, c("cluster", "subjectkey")]
        pval <- signif(ord_reg_p(cluster_df, outcome_df, outcome), 2)
        if (bonferroni) {
            pval <- pval * length(outcomes)
        }
        pval <- format(min(pval, 1), scientific = FALSE)
        #ord_reg_df <- rbind(ord_reg_df, c(outcome, pval))
        ord_reg_df[nrow(ord_reg_df) + 1, ] <- c(outcome, pval)
    }
    return(ord_reg_df)
}

#' Calculate overall p-values for an om
#'
#' @param om an output matrix
#' @param cbcl_list a list of CBCL measures
#' @param bonferroni boolean for reporting bonferroni corrected p-values
#'
#' @export
cbcl_ord_reg_from_om <- function(om, cbcl_list, bonferroni = FALSE) {
    ord_p_vals <- list()
    for (row in seq_len(nrow(om))) {
        current_row <- om[row, ]
        ord_p_vals
        cluster_df <- get_cluster_df(current_row)
        cluster_cbcl_list <- append(list(cluster_df), cbcl_list)
        characterization_df <- merge_df_list(cluster_cbcl_list)
        outcomes <- characterization_df |>
            dplyr::select(dplyr::starts_with("cbcl")) |>
            colnames()
        ord_reg_df <- data.frame(
            outcome = as.character(),
            pval = as.numeric())
        for (outcome in outcomes) {
            outcome_df <- characterization_df[, c("subjectkey", outcome)]
            cluster_df <- characterization_df[, c("cluster", "subjectkey")]
            pval <- signif(ord_reg_p(cluster_df, outcome_df, outcome), 2)
            if (bonferroni) {
                pval <- pval * length(outcomes)
            }
            pval <- format(min(pval, 1), scientific = FALSE)
            ord_reg_df[nrow(ord_reg_df) + 1, ] <- c(outcome, pval)
        }
        ord_p_vals <- append(ord_p_vals, list(ord_reg_df))
    }
    names(ord_p_vals) <- om$"significance"
    return(ord_p_vals)
}

#' Calculate anova p-values for a characterization_df
#'
#' @param characterization_df A merged list containing cluster, subjectkey, and
#'  various CBCL outcomes
#' @param bonferroni boolean for reporting bonferroni corrected p-values
#'
#' @export
cbcl_anova <- function(characterization_df, bonferroni = FALSE) {
    characterization_df$"cluster" <- as.factor(characterization_df$"cluster")
    outcomes <- characterization_df |>
        dplyr::select(dplyr::starts_with("cbcl")) |>
        colnames()
    print("ANOVA p-values:")
    print("---------------")
    for (outcome in outcomes) {
        aov_summary <- summary(stats::aov(
            characterization_df[, outcome] ~ characterization_df[, "cluster"]))
        pval <- aov_summary[[1]][["Pr(>F)"]][1]
        if (bonferroni) {
            pval <- pval * length(outcomes)
        }
        pval <- min(pval, 1)
        print(paste0(outcome, ": ", signif(pval, 2)))
    }
}

#' Select the top output matrix rows for each cluster
#'
#' Given an output matrix, returns a dataframe containing the row with the
#' lowest mean p-value and lowest min p-value for cluster sizes 2-5
#'
#' @param om an output matrix
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
#' @param om_row An output matrix row
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
    # Comparing training subjects between data list and output matrix...
    current_row_names <- subs(om_row) |>
        dplyr::select(dplyr::starts_with("subject_")) |>
        colnames()
    current_check <-
        identical(data_list[[i]]$"data"$"subjectkey"[train_indices],
                  current_row_names)
    if (current_check == FALSE) {
        print("Mismatch found between OM colnames and data list subject order.")
        return(NULL)
    }
    all_checks_passed <- TRUE
    return(all_checks_passed)
}
