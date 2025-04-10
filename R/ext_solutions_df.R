#' Constructor for `ext_solutions_df` class object
#'
#' The extended solutions data frame is a column-extended variation of the
#' solutions data frame. It contains association p-values relating cluster
#' membership to feature distribution for all solutions in a solutions data
#' frame and all features in a provided data list (or data lists). If a
#' target data list was used during the call to `extend_solutions`, the 
#' extended solutions data frame will also have columns "min_pval",
#' "mean_pval", and "max_pval" summarizing the p-values of just those features
#' that were a part of the target list.
#'
#' @keywords internal
#' @inheritParams extend_solutions
#' @param ext_sol_dfl An extended solutions data frame-like object.
#' @param fts A vector of all features that have association p-values stored
#'  in the resulting extended solutions data frame.
#' @return An `ext_solutions_df` class object.
ext_solutions_df <- function(ext_sol_dfl, sol_df, fts, target_dl) {
    ext_sol_dfl <- numcol_to_numeric(ext_sol_dfl)
    ext_sol_dfl <- dplyr::inner_join(sol_df, ext_sol_dfl, by = "solution")
    if (!is.null(target_dl)) {
        ordered_cols <- c(
            "solution", "nclust", "mc", "min_pval", "mean_pval", "max_pval",
            colnames(ext_sol_dfl)
        ) |>
            unique()
        ext_sol_dfl <- ext_sol_dfl[, ordered_cols]
        attributes(ext_sol_dfl)$"summary_features" <- features(target_dl)
    } else {
        attributes(ext_sol_dfl)$"summary_features" <- as.character()
    }
    attributes(ext_sol_dfl)$"features" <- fts
    attributes(ext_sol_dfl)$"snf_config" <- attributes(sol_df)$"snf_config"
    attributes(ext_sol_dfl)$"sim_mats_list" <- attributes(sol_df)$"sim_mats_list"
    ext_sol_dfl <- validate_ext_solutions_df(ext_sol_dfl)
    ext_sol_df <- new_ext_solutions_df(ext_sol_dfl)
    return(ext_sol_df)
}

#' Validator for `ext_solutions_df` class object
#'
#' @keywords internal
#' @param ext_sol_dfl An extended solutions data frame-like object.
#' @return If ext_sol_dfl has a valid structure for an object of class
#'  ext_solutions_df, returns ext_sol_dfl. Otherwise, raises an error.
validate_ext_solutions_df <- function(ext_sol_dfl) {
    class(ext_sol_dfl)  <- setdiff(class(ext_sol_dfl), "ext_solutions_df")
    pval_cols <- grep("_pval$", names(ext_sol_dfl), value = TRUE)
    pval_cols <- ext_sol_dfl[, pval_cols]
    if (!(length(pval_cols) > 1)) {
        metasnf_error(
            "Extended solutions data frame must have at least one p-value",
            " column."
        )
    }
    if (is.null(sim_mats_list(ext_sol_dfl))) {
        metasnf_error(
            "Extended solutions data frame must have `sim_mats_list`",
            " attribute."
        )
    }
    key_cols <- c("solution", "nclust", "mc")
    if (!identical(colnames(ext_sol_dfl)[1:3], key_cols)) {
        metasnf_error(
            "First three columns of `ext_solutions_df` must be \"solution\",",
            " \"nclust\", and \"mc\"."
        )
    }
    if (sum(grepl("^uid_", colnames(ext_sol_dfl))) == 0) {
        metasnf_error(
            "`ext_solutions_df` must have at least one observation (uid) column."
        )
    }
    if (nrow(ext_sol_dfl) == 0) {
        metasnf_error(
            "`ext_solutions_df` must have at least one row."
        )
    }
    return(ext_sol_dfl)
}

#' Constructor for `ext_solutions_df` class object
#'
#' @keywords internal
#' @inheritParams validate_ext_solutions_df 
#' @return An `ext_solutions_df` object, which is a data frame with class
#'  `ext_solutions_df`.
new_ext_solutions_df <- function(ext_sol_dfl) {
    pval_cols <- colnames(ext_sol_dfl)[grep("_pval$", colnames(ext_sol_dfl))]
    feature_cols <- setdiff(pval_cols, c("min_pval", "mean_pval", "max_pval"))
    fts <- gsub("_pval", "", feature_cols)
    attr(ext_sol_dfl, "features") <- fts
    ext_sol_df <- structure(ext_sol_dfl, class = c("ext_solutions_df", "data.frame"))
    return(ext_sol_df)
}

#' Extend a solutions data frame to include outcome evaluations
#'
#' @param sol_df Result of `batch_snf` storing cluster solutions and
#'  the settings that were used to generate them.
#' @param target_dl A data list with features to calculate p-values for.
#'  Features in the target list will be included during p-value summary
#'  measure calculations.
#' @param dl A data list with features to calculate p-values for, but
#'  that should not be incorporated into p-value summary measure columns (i.e.,
#'  min/mean/max p-value columns).
#' @param cat_test String indicating which statistical test will be used to
#'  associate cluster with a categorical feature. Options are "chi_squared" for
#'  the Chi-squared test and "fisher_exact" for Fisher's exact test.
#' @param min_pval If assigned a value, any p-value less than this will be
#'  replaced with this value.
#' @param processes The number of processes to use for parallelization.
#'  Progress is only reported for sequential processing (processes = 1).
#' @return An extended solutions data frame (`ext_sol_df` class object)
#'  that contains p-value columns for each outcome in the provided data lists
#' @param verbose If TRUE, output progress to console.
#' @export
#' @examples
#' \dontrun{
#'     input_dl <- data_list(
#'         list(gender_df, "gender", "demographics", "categorical"),
#'         list(diagnosis_df, "diagnosis", "clinical", "categorical"),
#'         uid = "patient_id"
#'     )
#'     
#'     sc <- snf_config(input_dl, n_solutions = 2)
#'     
#'     sol_df <- batch_snf(input_dl, sc)
#'     
#'     ext_sol_df <- extend_solutions(sol_df, input_dl)
#' }
extend_solutions <- function(sol_df,
                             target_dl = NULL,
                             dl = NULL,
                             cat_test = "chi_squared",
                             min_pval = 1e-10,
                             processes = 1,
                             verbose = FALSE) {
    # Remove nclust = 1 solutions
    single_cluster_solutions <- which(sol_df$"nclust" == 1)
    if (length(single_cluster_solutions) > 0) {
        metasnf_warning(
            "Single-cluster solution rows removed: ",
            single_cluster_solutions
        )
        sol_df <- sol_df[-c(single_cluster_solutions), ]
    }
    ###########################################################################
    # If data list and target list both exist, merge them
    ###########################################################################
    if (!is.null(dl) & !is.null(target_dl)) {
        dl <- c(dl, target_dl)
    } else {
        if (is.null(dl)) {
            dl <- target_dl
        }
    }
    ###########################################################################
    # Check to see if the dl and sol_df have matching observations
    ###########################################################################
    solution_obs <- uids(sol_df)
    dl_obs <- uids(dl)
    if (!identical(solution_obs, dl_obs)) {
        metasnf_error(
            "Observations in data list/target list do not match those in",
            " sol_df."
        )
    }
    ###########################################################################
    # Calculate vector of all feature names
    fts <- features(dl)
    n_fts <- length(fts)
    ###########################################################################
    # Calculate vector of all feature types
    ###########################################################################
    feature_types <- summary(dl, "feature")$"type"
    ###########################################################################
    # Construct base of extended solutions data frame by adding columns for
    # p-values of all fts
    ###########################################################################
    # Specifying the data frame structure avoids tibble-related errors
    ext_sol_dfl <- data.frame(matrix(NA, nrow = nrow(sol_df), ncol = 1 + n_fts))
    colnames(ext_sol_dfl) <- c("solution", paste0(fts, "_pval"))
    ###########################################################################
    # Single DF to contain all features to calculate p-values for
    ###########################################################################
    merged_df <- merge_df_list(lapply(dl, function(x) x$"data"))
    ###########################################################################
    # Sequential extension
    ###########################################################################
    if (processes == 1) {
        # Iterate across rows of the solutions data frame
        for (i in seq_len(nrow(sol_df))) {
            if (verbose) {
                cat("Processing row ", i, " of ", nrow(sol_df), "\n", sep = "")
            }
            clustered_obs <- t(sol_df[i, ])
            for (j in seq_along(fts)) {
                current_component_df <- merged_df[, c(1, j + 1)]
                current_ft <- colnames(current_component_df)[2]
                suppressWarnings({
                    pval <- calc_assoc_pval(
                        clustered_obs[, 2],
                        current_component_df[, 2],
                        "categorical",
                        feature_types[j],
                        cat_test = cat_test
                    )
                })
                target_col <- which(
                    paste0(current_ft, "_pval") == colnames(ext_sol_dfl)
                )
                ext_sol_dfl[[target_col]][i] <- pval
            }
        }
    } else {
        ########################################################################
        ## Parallel extension
        ########################################################################
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
        # Iterate across rows of the solutions data frame
        future::plan(future::multisession, workers = processes)
        ext_sol_dfl_rows <- future.apply::future_lapply(
            seq_len(nrow(ext_sol_dfl)),
            function(i) {
                clustered_obs <- t(sol_df[i, ])
                for (j in seq_along(fts)) {
                    current_component_df <- merged_df[, c(1, j + 1)]
                    current_ft <- colnames(current_component_df)[2]
                    evaluation_df <- dplyr::inner_join(
                        clustered_obs,
                        current_component_df,
                        by = "uid"
                    )
                    suppressWarnings({
                        pval <- calc_assoc_pval(
                            evaluation_df[, 2],
                            evaluation_df[, fts[j]],
                            "categorical",
                            feature_types[j],
                            cat_test = cat_test
                        )
                    })
                    target_col <- which(
                        paste0(current_ft, "_pval") == colnames(ext_sol_dfl)
                    )
                    ext_sol_dfl[[target_col]][i] <- pval
                }
                return(ext_sol_dfl[i, ])
            }
        )
        future::plan(future::sequential)
        ext_sol_dfl <- do.call("rbind", ext_sol_dfl_rows)
    }
    ext_sol_dfl$"solution" <- sol_df$"solution"
    reordered_cols <- unique(c("solution", colnames(ext_sol_dfl)))
    ext_sol_dfl <- ext_sol_dfl[, reordered_cols]
    ###########################################################################
    # If min_pval is assigned, use to replace any smaller p-value
    ###########################################################################
    if (!is.null(min_pval)) {
        ext_sol_dfl <- ext_sol_dfl |>
            numcol_to_numeric() |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::ends_with("_pval"),
                    ~ ifelse(. < min_pval, min_pval, .)
                )
            )
    }
    if (!is.null(target_dl)) {
        #######################################################################
        # Identify features present in target list
        #######################################################################
        target_fts <- summary(target_dl, scope = "feature")$"name"
        target_fts <- paste0(target_fts, "_pval")
        target_ext_sol_dfl <- ext_sol_dfl[, unique(c("solution", target_fts))]
        target_ext_sol_dfl <- summarize_pvals(target_ext_sol_dfl)
        target_ext_sol_dfl <- target_ext_sol_dfl[, !colnames(target_ext_sol_dfl) == "solution"]
        ext_sol_dfl <- ext_sol_dfl[, !colnames(ext_sol_dfl) %in% target_fts, drop = FALSE]
        ext_sol_dfl <- cbind(ext_sol_dfl, target_ext_sol_dfl)
    }
    ext_sol_df <- ext_solutions_df(ext_sol_dfl, sol_df, fts, target_dl)
    return(ext_sol_df)
}
