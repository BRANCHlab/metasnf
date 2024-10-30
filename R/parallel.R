#' Parallel processing form of batch_snf
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @param distance_metrics_list An optional nested list containing which
#' distance metric function should be used for the various feature types
#' (continuous, discrete, ordinal, categorical, and mixed). See
#' ?generate_distance_metrics_list for details on how to build this.
#'
#' @param clust_algs_list List of custom clustering algorithms to apply
#' to the final fused network. See ?generate_clust_algs_list.
#'
#' @param settings_matrix matrix indicating parameters to iterate SNF through.
#'
#' @param weights_matrix A matrix containing feature weights to use during
#'  distance matrix calculation. See ?generate_weights_matrix for details on
#'  how to build this.
#'
#' @param return_similarity_matrices If TRUE, function will return a list where
#'  the first element is the solutions matrix and the second element is a list
#'  of similarity matrices for each row in the solutions_matrix. Default FALSE.
#'
#' @param similarity_matrix_dir If specified, this directory will be used to
#'  save all generated similarity matrices.
#'
#' @param processes Number of parallel processes used when executing SNF.
#'
#' @return The same values as ?batch_snf().
#'
#' @export
parallel_batch_snf <- function(data_list,
                               distance_metrics_list,
                               clust_algs_list,
                               settings_matrix,
                               weights_matrix,
                               similarity_matrix_dir,
                               return_similarity_matrices,
                               processes) {
    future::plan(future::multisession, workers = processes)
    ############################################################################
    settings_and_weights_df <- cbind(settings_matrix, weights_matrix)
    prog <- progressr::progressor(steps = nrow(settings_and_weights_df))
    batch_row_function <- batch_row_closure(
        data_list = data_list,
        distance_metrics_list = distance_metrics_list,
        clust_algs_list = clust_algs_list,
        settings_matrix = settings_matrix,
        weights_matrix = weights_matrix,
        similarity_matrix_dir = similarity_matrix_dir,
        return_similarity_matrices = return_similarity_matrices,
        prog = prog
    )
    batch_snf_results <- future.apply::future_apply(
        settings_and_weights_df,
        1,
        batch_row_function
    )
    ############################################################################
    future::plan(future::sequential)
    if (return_similarity_matrices) {
        solutions_matrix_rows <- batch_snf_results |>
            lapply(
                function(x) {
                    x$"solutions_matrix_row"
                }
            )
        solutions_matrix <- do.call("rbind", solutions_matrix_rows)
        solutions_matrix <- numcol_to_numeric(solutions_matrix)
        similarity_matrices <- batch_snf_results |>
            lapply(
                function(x) {
                    x$"similarity_matrix"
                }
            )
        batch_snf_results <- list(
            "solutions_matrix" = solutions_matrix,
            "similarity_matrices" = similarity_matrices
        )
        return(batch_snf_results)
    } else {
        solutions_matrix <- do.call("rbind", batch_snf_results)
        solutions_matrix <- numcol_to_numeric(solutions_matrix)
        return(solutions_matrix)
    }
}

#' Generate closure function to run batch_snf in an apply-friendly format
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @param distance_metrics_list An optional nested list containing which
#' distance metric function should be used for the various feature types
#' (continuous, discrete, ordinal, categorical, and mixed). See
#' ?generate_distance_metrics_list for details on how to build this.
#'
#' @param clust_algs_list List of custom clustering algorithms to apply
#' to the final fused network. See ?generate_clust_algs_list.
#'
#' @param settings_matrix matrix indicating parameters to iterate SNF through.
#'
#' @param weights_matrix A matrix containing feature weights to use during
#'  distance matrix calculation. See ?generate_weights_matrix for details on
#'  how to build this.
#'
#' @param return_similarity_matrices If TRUE, function will return a list where
#'  the first element is the solutions matrix and the second element is a list
#'  of similarity matrices for each row in the solutions_matrix. Default FALSE.
#'
#' @param similarity_matrix_dir If specified, this directory will be used to
#'  save all generated similarity matrices.
#'
#' @param prog Progressr function to update parallel processing progress
#'
#' @return A "function" class object used to run `batch_snf` in lapply-form
#' for parallel processing.
#'
#' @export
batch_row_closure <- function(data_list,
                              distance_metrics_list,
                              clust_algs_list,
                              settings_matrix,
                              weights_matrix,
                              similarity_matrix_dir,
                              return_similarity_matrices,
                              prog) {
    settings_matrix_names <- colnames(settings_matrix)
    weights_matrix_names <- colnames(weights_matrix)
    row_function <- function(settings_and_weights_row) {
        prog()
        settings_and_weights_row_df <- data.frame(t(settings_and_weights_row))
        settings_matrix_row <-
            settings_and_weights_row_df[, settings_matrix_names]
        weights_row <- settings_and_weights_row_df[, weights_matrix_names]
        # Reduce data list
        current_data_list <- drop_inputs(settings_matrix_row, data_list)
        # Extract parameters for snf_step
        current_snf_scheme <- dplyr::case_when(
            settings_matrix_row$"snf_scheme" == 1 ~ "individual",
            settings_matrix_row$"snf_scheme" == 2 ~ "domain",
            settings_matrix_row$"snf_scheme" == 3 ~ "twostep",
        )
        k <- settings_matrix_row$"k"
        alpha <- settings_matrix_row$"alpha"
        t <- settings_matrix_row$"t"
        cont_dist <- settings_matrix_row$"cont_dist"
        disc_dist <- settings_matrix_row$"disc_dist"
        ord_dist <- settings_matrix_row$"ord_dist"
        cat_dist <- settings_matrix_row$"cat_dist"
        mix_dist <- settings_matrix_row$"mix_dist"
        cont_dist_fn <- distance_metrics_list$"continuous_distance"[[cont_dist]]
        disc_dist_fn <- distance_metrics_list$"discrete_distance"[[disc_dist]]
        ord_dist_fn <- distance_metrics_list$"ordinal_distance"[[ord_dist]]
        cat_dist_fn <- distance_metrics_list$"categorical_distance"[[cat_dist]]
        mix_dist_fn <- distance_metrics_list$"mixed_distance"[[mix_dist]]
        # Integrate data
        fused_network <- snf_step(
            current_data_list,
            current_snf_scheme,
            k = k,
            alpha = alpha,
            t = t,
            cont_dist_fn = cont_dist_fn,
            disc_dist_fn = disc_dist_fn,
            ord_dist_fn = ord_dist_fn,
            cat_dist_fn = cat_dist_fn,
            mix_dist_fn = mix_dist_fn,
            weights_row = weights_row
        )
        # Write similarity matrices if requested
        if (!is.null(similarity_matrix_dir)) {
            row_id <- settings_matrix_row$"row_id"
            utils::write.csv(
                x = fused_network,
                file = similarity_matrix_path(similarity_matrix_dir, row_id),
                row.names = TRUE
            )
        }
        clust_alg <- clust_algs_list[[settings_matrix_row$"clust_alg"]]
        # cluster_results is a named list containing the cluster solution
        #  (vector of which cluster each patient was assigned to) and the
        #  number of clusters for that solution
        cluster_results <- clust_alg(fused_network)
        solution <- cluster_results$"solution"
        nclust <- cluster_results$"nclust"
        solutions_matrix_row <- settings_matrix_row
        solutions_matrix_row$"nclust" <- nclust
        solutions_matrix_row[1, rownames(fused_network)] <- solution
        if (return_similarity_matrices) {
            batch_snf_results <- list(
                "solutions_matrix_row" = solutions_matrix_row,
                "similarity_matrix" = fused_network
            )
            return(batch_snf_results)
        } else {
            return(solutions_matrix_row)
        }
    }
    return(row_function)
}
