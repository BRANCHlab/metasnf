#' Meta-cluster calculations
#'
#' Generate matrix of pairwise cluster-solution similarities by Adjusted Rand
#'  index calculations.
#'
#' @param om solutions_matrix to calculate pairwise ARIs for.
#' @param processes Specify number of processes used to complete calculations
#'  * `1` (default) Sequential processing
#'  * `2` or higher: Parallel processing will use the
#'    `future.apply::future_apply` to distribute the calculations across
#'    the specified number of CPU cores. If higher than the number of
#'    available cores, a warning will be printed and the maximum number of
#'    cores will be used.
#'  * `max`: All available cores will be used.
#' Note that no progress indicator is available during multi-core processing.
#'
#' @return om_aris ARIs between clustering solutions of an solutions matrix
#'
#' @export
calc_om_aris <- function(om,
                         processes = 1) {
    ###########################################################################
    # Prepare dataframe containing 1 cluster solution per row
    ###########################################################################
    # Only row id and subject label columns
    om_subs <- subs(om)
    # Only subject label cols
    om_no_id <- as.matrix(om_subs[, 2:length(om_subs)])
    # The skeleton of the inter-cluster similarity matrix
    om_aris <- matrix(1, nrow(om_subs), nrow(om_subs))
    ###########################################################################
    # Indices of all pairwise comparisons to calculate ARIs for
    ###########################################################################
    pairwise_indices <- utils::combn(nrow(om_aris), 2)
    ###########################################################################
    # Run calculations (sequentially or in parallel)
    ###########################################################################
    if (processes == 1) {
        for (col in seq_len(ncol(pairwise_indices))) {
            if (col %% 100 == 0) {
                progress <- 100 * col / ncol(pairwise_indices)
                cat("\r", progress, "% completed...", sep = "")
            }
            v1 <- pairwise_indices[1, col]
            v2 <- pairwise_indices[2, col]
            ari <- mclust::adjustedRandIndex(om_no_id[v1, ], om_no_id[v2, ])
            om_aris[v1, v2] <- ari
            om_aris[v2, v1] <- ari
        }
        colnames(om_aris) <- om$"row_id"
        rownames(om_aris) <- om$"row_id"
        cat("\r")
        cat("\n")
        cat("\r", "Done.", sep = "")
        cat("\n")
        return(om_aris)
    } else {
        max_cores <- future::availableCores()
        if (processes == "max") {
            processes <- max_cores
        } else if (processes > max_cores) {
            print(
                paste0(
                    "Requested processes exceed available cores.",
                    " Defaulting to the max avaiilable (", max_cores, ")."
                )
            )
            processes <- max_cores
        }
        # Parallelized ARI calculations
        future::plan(future::multisession, workers = processes)
        ari_vector <- future.apply::future_apply(
            pairwise_indices,
            MARGIN = 2,
            FUN = function(col) {
                mclust::adjustedRandIndex(
                    om_no_id[col[1], ],
                    om_no_id[col[2], ]
                )
            }
        )
        future::plan(future::sequential)
        #######################################################################
        # Formatting of results to symmetric matrix
        #######################################################################
        om_aris[lower.tri(om_aris, diag = FALSE)] <- ari_vector
        om_aris <- t(om_aris)
        om_aris[lower.tri(om_aris)] <- t(om_aris)[lower.tri(om_aris)]
        colnames(om_aris) <- om$"row_id"
        rownames(om_aris) <- om$"row_id"
        return(om_aris)
    }
}

#' Return the row ordering of a meta-clustering solution
#'
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
