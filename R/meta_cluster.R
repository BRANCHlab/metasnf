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
