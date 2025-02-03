#' Parallel processing form of batch_snf
#'
#' @keywords internal
#' @param dl A data list.
#' @param dfl An optional nested list containing which
#'  distance metric function should be used for the various feature types
#'  (continuous, discrete, ordinal, categorical, and mixed). See
#'  ?dist_fns_list for details on how to build this.
#' @param cfl List of custom clustering algorithms to apply
#' to the final fused network. See ?clust_fns_list.
#' @param sdf matrix indicating parameters to iterate SNF through.
#' @param wm A matrix containing feature weights to use during
#'  distance matrix calculation. See ?weights_matrix for details on
#'  how to build this.
#' @param return_sim_mats If TRUE, function will return a list where
#'  the first element is the solutions data frame and the second element is a list
#'  of similarity matrices for each row in the sol_df. Default FALSE.
#' @param similarity_matrix_dir If specified, this directory will be used to
#'  save all generated similarity matrices.
#' @param processes Number of parallel processes used when executing SNF.
#' @return The same values as ?batch_snf().
parallel_batch_snf <- function(dl,
                               dfl,
                               cfl,
                               sdf,
                               wm,
                               similarity_matrix_dir,
                               return_sim_mats,
                               processes) {
    future::plan(future::multisession, workers = processes)
    ############################################################################
    settings_and_weights_df <- cbind(sdf, wm)
    prog <- progressr::progressor(steps = nrow(settings_and_weights_df))
    batch_row_function <- batch_row_closure(
        dl = dl,
        dfl = dfl,
        cfl = cfl,
        sdf = sdf,
        wm = wm,
        similarity_matrix_dir = similarity_matrix_dir,
        return_sim_mats = return_sim_mats,
        prog = prog
    )
    batch_snf_results <- future.apply::future_apply(
        settings_and_weights_df,
        1,
        batch_row_function
    )
    ############################################################################
    future::plan(future::sequential)
    if (return_sim_mats) {
        sol_df_rows <- batch_snf_results |>
            lapply(
                function(x) {
                    x$"sol_df_row"
                }
            )
        sol_df <- data.frame(do.call("rbind", sol_df_rows))
        sol_df <- numcol_to_numeric(sol_df)
        similarity_matrices <- batch_snf_results |>
            lapply(
                function(x) {
                    x$"similarity_matrix"
                }
            )
        batch_snf_results <- list(
            "sol_df" = sol_df,
            "similarity_matrices" = similarity_matrices
        )
        return(batch_snf_results)
    } else {
        sol_df <- data.frame(do.call("rbind", batch_snf_results))
        sol_df <- numcol_to_numeric(sol_df)
        return(sol_df)
    }
}

#' Generate closure function to run batch_snf in an apply-friendly format
#'
#' @keywords internal
#' @param dl A nested list of input data from `data_list()`.
#' @param dfl An optional nested list containing which
#'  distance metric function should be used for the various feature types
#'  (continuous, discrete, ordinal, categorical, and mixed). See
#'  ?dist_fns_list for details on how to build this.
#' @param cfl List of custom clustering algorithms to apply
#'  to the final fused network. See ?clust_fns_list.
#' @param sdf matrix indicating parameters to iterate SNF through.
#' @param wm A matrix containing feature weights to use during
#'  distance matrix calculation. See ?weights_matrix for details on
#'  how to build this.
#' @param return_sim_mats If TRUE, function will return a list where
#'  the first element is the solutions data frame and the second element is a list
#'  of similarity matrices for each row in the sol_df. Default FALSE.
#' @param similarity_matrix_dir If specified, this directory will be used to
#'  save all generated similarity matrices.
#' @param prog Progressr function to update parallel processing progress
#' @return A "function" class object used to run `batch_snf` in lapply-form
#'  for parallel processing.
batch_row_closure <- function(dl,
                              dfl,
                              cfl,
                              sdf,
                              wm,
                              similarity_matrix_dir,
                              return_sim_mats,
                              prog) {
    sdf_names <- colnames(sdf)
    wm_names <- colnames(wm)
    row_function <- function(settings_and_weights_row) {
        prog()
        settings_and_weights_row_df <- data.frame(t(settings_and_weights_row))
        sdf_row <- settings_and_weights_row_df[, sdf_names] |>
            as_settings_df()
        weights_row <- settings_and_weights_row_df[, wm_names]
        # Reduce data list
        current_dl <- drop_inputs(sdf_row, dl)
        # Extract parameters for snf_step
        k <- sdf_row$"k"
        alpha <- sdf_row$"alpha"
        t <- sdf_row$"t"
        cnt_dist <- sdf_row$"cnt_dist"
        dsc_dist <- sdf_row$"dsc_dist"
        ord_dist <- sdf_row$"ord_dist"
        cat_dist <- sdf_row$"cat_dist"
        mix_dist <- sdf_row$"mix_dist"
        cnt_dist_fn <- dfl$"cnt_dist_fns"[[cnt_dist]]
        dsc_dist_fn <- dfl$"dsc_dist_fns"[[dsc_dist]]
        ord_dist_fn <- dfl$"ord_dist_fns"[[ord_dist]]
        cat_dist_fn <- dfl$"cat_dist_fns"[[cat_dist]]
        mix_dist_fn <- dfl$"mix_dist_fns"[[mix_dist]]
        # Integrate data
        fused_network <- snf_step(
            current_dl,
            scheme = sdf_row$"snf_scheme",
            k = k,
            alpha = alpha,
            t = t,
            cnt_dist_fn = cnt_dist_fn,
            dsc_dist_fn = dsc_dist_fn,
            ord_dist_fn = ord_dist_fn,
            cat_dist_fn = cat_dist_fn,
            mix_dist_fn = mix_dist_fn,
            weights_row = weights_row
        )
        # Write similarity matrices if requested
        if (!is.null(similarity_matrix_dir)) {
            solution <- sdf_row$"solution"
            utils::write.csv(
                x = fused_network,
                file = similarity_matrix_path(similarity_matrix_dir, solution),
                row.names = TRUE
            )
        }
        clust_alg <- cfl[[sdf_row$"clust_alg"]]
        # cluster_results is a named list containing the cluster solution
        #  (vector of which cluster each patient was assigned to) and the
        #  number of clusters for that solution
        solution <- clust_alg(fused_network)
        nclust <- length(unique(solution))
        sol_df_row <- sdf_row
        sol_df_row$"nclust" <- nclust
        sol_df_row[1, rownames(fused_network)] <- solution
        if (return_sim_mats) {
            batch_snf_results <- list(
                "sol_df_row" = sol_df_row,
                "similarity_matrix" = fused_network
            )
            return(batch_snf_results)
        } else {
            return(sol_df_row)
        }
    }
    return(row_function)
}
