#' (DEPRECATED) Function for label propagating results in a solutions_matrix
#'
#' @param solutions_matrix A solutions_matrix (training set only)
#' @param full_data_list A data_list containing training and testing subjects
#' @param clust_algs_list The clustering algorithms list used to create the
#' original solutions matrix (if any was used)
#' @param distance_metrics_list The distance metrics list used to create the
#' original solutions matrix (if any was used)
#' @param weights_matrix The weights matrix used to create the original
#' solutions matrix (if any was used)
#'
#' @export
lp_row <- function(solutions_matrix,
                   full_data_list,
                   clust_algs_list = NULL,
                   distance_metrics_list = NULL,
                   weights_matrix = NULL) {
    stop("This function is deprecated. Use lp_solutions_matrix instead.")
}

#' (DEPRECATED) Select p-values from solutions matrix
#' Replaced with "pval_select'
#'
#' @param solutions_matrix The output of batch_snf
#'
#' @return p_val_matrix P-values ready for heatmap plotting
#'
#' @export
p_val_select <- function(solutions_matrix) {
    warning("This function has been replaced by `pval_select`.")
    return(pval_select(solutions_matrix))
}

#' (DEPRECATED) Select p-values from an extended solutions matrix
#'
#' This function can be used to neatly format the p-values associated with an
#' extended solutions matrix. It can also calculate the negative logs of those
#' p-values to make it easier to interpret large-scale differences.
#'
#' @param extended_solutions_matrix The output of `extend_solutions`. A
#' dataframe that contains at least one p-value column ending in "_p".
#' @param negative_log If TRUE, will replace p-values with negative log
#' p-values.
#'
#' @export
pval_select <- function(extended_solutions_matrix,
                        negative_log = FALSE) {
    message(
        "This function has been replaced by `get_pvals`.",
        " The updated function no longer preserves or manages pval_summaries.",
        " To obtain pvalue summaries (min/mean/max), see the `pval_summaries`",
        " function."
    )
    # Select p-value columns and convert to numeric
    pval_df <- extended_solutions_matrix |>
        dplyr::select(
            "row_id",
            dplyr::ends_with("_p"),
            dplyr::contains("p_val")
        ) |>
        data.frame() |>
        metasnf::numcol_to_numeric()
    # Convert p-values to negative log p-values if requested
    if (negative_log) {
        # Remove summary columns from non-negative log p-value calculations
        pval_df <- dplyr::select(
            pval_df,
            -dplyr::contains(c("min_p_val", "mean_p_val"))
        )
        # Negative log conversions
        neg_log_pval_df <- -log(pval_df)
        neg_log_pval_df$"row_id" <- pval_df$"row_id"
        pval_df <- neg_log_pval_df
        mini_df <- pval_df |> dplyr::select(
            dplyr::ends_with("_p")
        )
        pval_df$"mean_neglog_p" <- apply(mini_df, 1, FUN = mean)
        pval_df$"max_neglog_p" <- apply(mini_df, 1, FUN = max)
    }
    return(pval_df)
}

#' (DEPRECATED) Add minimum and mean p-values to an extended solutions matrix
#'
#' @param solutions_matrix A solutions_matrix object that already has some
#' p-value columns included.
#'
#' @param na_rm If TRUE, NA values will be removed for mean/min calculations
#'
#' @export
pval_summaries <- function(solutions_matrix, na_rm = TRUE) {
    pval_cols <- solutions_matrix |>
        dplyr::select(dplyr::ends_with("_p"))
    pval_cols <- numcol_to_numeric(pval_cols)
    if (na_rm) {
        mean_pvals <- apply(
            pval_cols,
            1,
            FUN = function(x) {
                mean(x, na.rm = TRUE)
            }
        )
        min_pvals <- apply(
            pval_cols,
            1,
            FUN = function(x) {
                min(x, na.rm = TRUE)
            }
        )
    } else {
        mean_pvals <- apply(pval_cols, 1, FUN = mean)
        min_pvals <- apply(pval_cols, 1, FUN = min)
    }
    solutions_matrix$"min_p" <- min_pvals
    solutions_matrix$"mean_p" <- mean_pvals
    return(solutions_matrix)
}
