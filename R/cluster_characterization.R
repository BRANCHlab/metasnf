#' Heatmap p-value matrix
#'
#' @param p_val_matrix matrix of p-values
#' @param file_path where to store heatmap
#'
#' @export
heatmap_pvals <- function(p_val_matrix, file_path = NA) {
    my_colors <- grDevices::colorRampPalette(c("cyan", "deeppink3"))
    pheatmap::pheatmap(p_val_matrix, col = rev(my_colors(100)),
        filename = file_path)
    pheatmap::pheatmap(p_val_matrix, col = rev(my_colors(100)))
}


#' Select all output_matrix columns except for subjects
#'
#' @description
#' Return a dataframe without any subject columns
#'
#' @param df Datframe
#'
#' @return df_no_subs Dataframe without subjects
#'
#' @export
no_subs <- function(df) {
    df_no_subs <- df |> dplyr::select(!(dplyr::starts_with("NDAR")))
    return(df_no_subs)
}


#' Select only output_matrix columns containing subjects
#'
#' @description
#' Return a dataframe with only subject columns
#'
#' @param df Datframe
#'
#' @return df_subs Dataframe without subjects
#'
#' @export
subs <- function(df) {
    df_subs <- df |> dplyr::select(
        "row_id",
        dplyr::starts_with("NDAR"))
    return(df_subs)
}

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


#' Calculate NMI scores for SNF inputs
#'
#' @param om_row An output matrix row
#' @param data_list A data list
#'
#' @return nmi_df A dataframe containing input NMI scores in descending order
#'
#' @export
calc_nmi <- function(om_row, data_list) {
    K <- om_row$"K"
    alpha <- om_row$"alpha"
    nclust <- om_row$"nclust"
    # Getting the clustering results of the full fused network
    full_solution <- as.numeric(subs(om_row)[, -1])
    # Getting the clustering results of each individual SNF input
    dist_list <- lapply(data_list,
        function(x) {
            get_dist_matrix(df = x$"data", input_type = x$"type")
        })
    sim_list <- lapply(dist_list,
        function(x) {
            SNFtool::affinityMatrix(x, K = K, sigma = alpha)
        })
    input_solutions <- lapply(sim_list,
        function(x) {
            SNFtool::spectralClustering(x, nclust)
        })
    nmi_scores <- lapply(input_solutions,
        function(x) {
            SNFtool::calNMI(full_solution, x)
        })
    nmi_scores <- format(signif(unlist(nmi_scores), 2), scientific = FALSE)
    nmi_df <- data.frame(input = sdl(data_list)$"name", nmi = nmi_scores)
    nmi_df <- nmi_df |> dplyr::arrange(dplyr::desc(nmi_df$"nmi"))
    return(nmi_df)
}
