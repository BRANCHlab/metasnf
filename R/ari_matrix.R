#' Construct an ARI matrix storing inter-solution similarities
#'
#' This function constructs an `ari_matrix` class object from a `solutions_df`
#' class object. The ARI matrix stores pairwise adjusted Rand indices for all
#' cluster solutions as well as a numeric order for the solutions data frame
#' based on the hierarchical clustering of the ARI matrix.
#'
#' @inheritParams get_matrix_order
#' @param sol_df Solutions data frame containing cluster solutions to calculate
#'  pairwise ARIs for.
#' @param processes Specify number of processes used to complete calculations
#'  * `1` (default) Sequential processing
#'  * `2` or higher: Parallel processing will use the
#'    `future.apply::future_apply` to distribute the calculations across
#'    the specified number of CPU cores. If higher than the number of
#'    available cores, a warning will be raised and the maximum number of
#'    cores will be used.
#'  * `max`: All available cores will be used.
#'  Note that no progress indicator is available during multi-core processing.
#' @param verbose If TRUE, output progress to console.
#' @return om_aris ARIs between clustering solutions of an solutions data frame
#' @export
#' @examples
#' dl <- data_list(
#'     list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
#'     list(pubertal, "pubertal_status", "demographics", "continuous"),
#'     uid = "unique_id"
#' )
#' 
#' sc <- snf_config(dl, n_solutions = 3)
#' sol_df <- batch_snf(dl, sc)
#' calc_aris(sol_df)
calc_aris <- function(sol_df,
                      processes = 1,
                      verbose = FALSE,
                      dist_method = "euclidean",
                      hclust_method = "complete") {
    if (nrow(sol_df) < 2) {
        metasnf_error(
            "ARIs can only be calculated from solutions data frames with more",
            " than one solution."
        )
    }
    ###########################################################################
    # Prepare data frame containing 1 cluster solution per row
    # Row id and uid columns
    uids <- drop_cols(sol_df, c("solution", "nclust", "mc"))
    # Only subject label cols
    uids_no_id <- as.matrix(uids[, 2:length(uids)])
    # The skeleton of the inter-cluster similarity matrix
    aris <- matrix(1, nrow(uids), nrow(uids))
    ###########################################################################
    # Indices of all pairwise comparisons to calculate ARIs for
    ###########################################################################
    pairwise_indices <- utils::combn(nrow(aris), 2)
    ###########################################################################
    # Run calculations (sequentially or in parallel)
    ###########################################################################
    if (processes == 1) {
        for (col in seq_len(ncol(pairwise_indices))) {
            if (verbose) {
                if (col %% 100 == 0) {
                    progress <- 100 * col / ncol(pairwise_indices)
                    cat(progress, "% completed...\n", sep = "")
                }
            }
            v1 <- pairwise_indices[1, col]
            v2 <- pairwise_indices[2, col]
            ari <- mclust::adjustedRandIndex(
                uids_no_id[v1, ],
                uids_no_id[v2, ]
            )
            aris[v1, v2] <- ari
            aris[v2, v1] <- ari
        }
        colnames(aris) <- sol_df$"solution"
        rownames(aris) <- sol_df$"solution"
        if (verbose) {
            cat("100% completed.\n")
        }
    } else {
        max_cores <- future::availableCores()
        if (processes == "max") {
            processes <- max_cores
        } else if (processes > max_cores) {
            metasnf_warning(
                "Requested processes exceed available cores.",
                " Defaulting to the max available (", max_cores, ")."
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
                    uids_no_id[col[1], ],
                    uids_no_id[col[2], ]
                )
            }
        )
        future::plan(future::sequential)
        #######################################################################
        # Formatting of results to symmetric matrix
        #######################################################################
        aris[lower.tri(aris, diag = FALSE)] <- ari_vector
        aris <- t(aris)
        aris[lower.tri(aris)] <- t(aris)[lower.tri(aris)]
        colnames(aris) <- sol_df$"solution"
        rownames(aris) <- sol_df$"solution"
    }
    aris <- validate_ari_matrix(aris)
    aris <- new_ari_matrix(
        aris,
        dist_method = dist_method,
        hclust_method = hclust_method
    )
    return(aris)
}

#' Validator for `ari_matrix` class object
#'
#' @keywords internal
#' @param aml An ari_matrix-like matrix object to be validated.
#' @return If aml has a valid structure for a `ari_matrix` class
#'  object, returns the input unchanged. Otherwise, raises an error.
validate_ari_matrix <- function(aml) {
    class(aml) <- setdiff(class(aml), "ari_matrix")
    # 1. Ensure is matrix
    if (!inherits(aml, "matrix")) {
        metasnf_error("`ari_matrix` must inherit from class `matrix`.")
    }
    # 2. Ensure is array
    if (!inherits(aml, "array")) {
        metasnf_error("`ari_matrix` must inherit from class `array`.")
    }
    # 3. Check for NAs
    if (any(is.na(aml))) {
        metasnf_error("`ari_matrix` cannot have missing values.")
    }
    return(aml)
}

#' Constructor for `ari_matrix` class object
#' 
#' @keywords internal
#' @inheritParams validate_ari_matrix
#' @return A `ari_matrix` object.
new_ari_matrix <- function(aml, dist_method, hclust_method) {
    attributes(aml)$"order" <- get_matrix_order(
        aml,
        dist_method = dist_method,
        hclust_method = hclust_method
    )
    am <- structure(aml, class = c("ari_matrix", "matrix", "array"))    
    return(am)
}
