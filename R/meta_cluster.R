#' Calculate adjusted rand index between two rows of output matrix
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


#' Meta-cluster calculations
#'
#' @description
#' Generate matrix of pairwise cluster-solution similarities by Adjusted Rand
#'  index calculations
#'
#' @param mc_om output_matrix
#'
#' @return mc_sm meta-clustering similarity matrix
#'
#' @export
meta_cluster <- function(mc_om) {
    # Only row id and subject label cols
    mc_om_subs <- subs(mc_om)
    # Only subject label cols
    mc_om_no_id <- mc_om_subs[, 2:length(mc_om_subs)]
    # The skeleton of the inter-cluster similarity matrix
    mc_sm <- matrix(0, nrow(mc_om_subs), nrow(mc_om_subs))
    # Calculating pairwise ARIs across rows
    for (i in seq_len(nrow(mc_sm))) {
        for (j in seq_len(ncol(mc_sm))) {
            print(i)
            print(j)
            mc_sm[i, j] <- calc_ari(i, j, mc_om_no_id)
        }
    }
    return(mc_sm)
}


#' Return the row ordering of a meta-clustering solution
#'
#' @description
#' Pheatmap reorders meta clustering results to enable meta-cluster
#'  visualization. This function extracts the new row orders to apply to other
#'  matrices.
#'
#' @param mc_results
#'
#' @return pheatmap_order Row orders of the clustered pheatmap
#'
#' @export
get_pheatmap_order <- function(matrix) {
    out <- pheatmap::pheatmap(matrix)
    pheatmap_order <- out$"tree_row"[["order"]]
    return(pheatmap_order)
}
