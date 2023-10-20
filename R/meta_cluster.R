#' Calculate adjusted rand index between two rows of solutions matrix
#'
#' Output matrix must have no row_id or non-sub columns
#'
#' @param r1 first row of cluster labels
#' @param r2 second row of cluster labels
#' @param df dataframe containing cluster labels
#'
#' @return ari adjusted rand index
#'
#' @export
calc_ari <- function(r1, r2, df) {
    v1 <- as.numeric(df[r1, ])
    v2 <- as.numeric(df[r2, ])
    ari <- mclust::adjustedRandIndex(v1, v2)
    return(ari)
}

#' Outdated calc_om_aris
#'
#' @param mc_om solutions matrix
#'
#' @export
meta_cluster <- function(mc_om) {
    print("The function is now named calc_om_aris.")
    return(calc_om_aris(mc_om))
}

#' Meta-cluster calculations
#'
#' @description
#' Generate matrix of pairwise cluster-solution similarities by Adjusted Rand
#'  index calculations
#'
#' @param om solutions_matrix
#'
#' @return om_aris ARIs between clustering solutions of an solutions matrix
#'
#' @export
calc_om_aris <- function(om) {
    print("Please wait - this may take a minute.")
    # Only row id and subject label cols
    om_subs <- subs(om)
    # Only subject label cols
    om_no_id <- om_subs[, 2:length(om_subs)]
    # The skeleton of the inter-cluster similarity matrix
    om_aris <- matrix(1, nrow(om_subs), nrow(om_subs))
    pairwise_indices <- utils::combn(nrow(om_aris), 2)
    # Calculating pairwise ARIs across rows
    for (col in seq_len(ncol(pairwise_indices))) {
        if (col %% 100 == 0) {
            progress <- 100 * col / ncol(pairwise_indices)
            cat("\r", progress, "% completed...", sep = "")
        }
        v1 <- pairwise_indices[1, col]
        v2 <- pairwise_indices[2, col]
        ari <- calc_ari(v1, v2, om_no_id)
        om_aris[v1, v2] <- ari
        om_aris[v2, v1] <- ari
    }
    colnames(om_aris) <- om$"row_id"
    rownames(om_aris) <- om$"row_id"
    return(om_aris)
}

#' Return the row ordering of a meta-clustering solution
#'
#' @description
#' Pheatmap reorders meta clustering results to enable meta-cluster
#'  visualization. This function extracts the new row orders to apply to other
#'  matrices.
#'
#' @param matrix matrix used as pheatmap input
#'
#' @return pheatmap_order Row orders of the clustered pheatmap
#'
#' @export
get_heatmap_order <- function(matrix) {
    out <- pheatmap::pheatmap(matrix)
    pheatmap_order <- out$"tree_row"[["order"]]
    return(pheatmap_order)
}

#' Outdated adjusted_rand_index_heatmap
#'
#' @param mc_results outdated
#' @param save outdated
#'
#' @export
mc_heatmap <- function(mc_results, save = NULL) {
    print("The new function name is adjusted_rand_index_heatmap")
    adjusted_rand_index_heatmap(mc_results, save)
}

#' Heatmap meta-clustering results
#'
#' @param solutions_matrix_aris results from meta_cluster function
#' @param title plot title
#' @param save optional path to save figure to
#' @param cluster_cols boolean indicating if columns shold be clustered
#' @param cluster_rows boolean indicating if rows shold be clustered
#' @param hide_columns boolean indicating if column names should be hidden
#' @param hide_rows boolean indicating if row names should be hidden
#' @param ... additional parameters to pass into pheatmap
#'
#' @export
adjusted_rand_index_heatmap <- function(solutions_matrix_aris,
                                        title = "",
                                        save = NULL,
                                        cluster_cols = TRUE,
                                        cluster_rows = TRUE,
                                        hide_columns = FALSE,
                                        hide_rows = FALSE,
                                        ...) {
    if (hide_columns) {
        colnames(solutions_matrix_aris) <- NULL
    }
    if (hide_rows) {
        rownames(solutions_matrix_aris) <- NULL
    }
    if (!(is.null(grDevices::dev.list()))) {
        grDevices::dev.off()
    }
    if (!(is.null(save))) {
        pheatmap::pheatmap(
            solutions_matrix_aris,
            legend_breaks = c(0, 0.5, 1, max(solutions_matrix_aris)),
            main = title,
            legend_labels = c("0", "0.5", "1", "ARI\n\n"),
            legend = TRUE,
            border_color = FALSE,
            cluster_cols = cluster_cols,
            cluster_rows = cluster_rows,
            filename = save,
            ...
        )
    }
    pheatmap::pheatmap(
        solutions_matrix_aris,
        legend_breaks = c(0, 0.5, 1, max(solutions_matrix_aris)),
        main = title,
        legend_labels = c("0", "0.5", "1", "ARI\n\n"),
        legend = TRUE,
        border_color = FALSE,
        cluster_cols = cluster_cols,
        cluster_rows = cluster_rows,
        ...
    )
}
