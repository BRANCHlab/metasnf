#' Constructor for `solutions_df` class object
#'
#' @param sol_dfl A solutions data frame-like object to be validated and
#'  converted into a solutions data frame.
#' @param smll A similarity matrix list-like object to be validated and used
#'  to construct a solutions data frame.
#' @param sc An `snf_config` object used to construct a solutions data frame.
#' @param dl An `data_list` object used to construct a solutions data frame.
#' @return A solutions data frame (`solutions_df` class object).
#' @export
solutions_df <- function(sol_dfl, smll, sc, dl) {
    smll <- validate_sim_mats_list(smll)
    sml <- new_sim_mats_list(smll)
    attributes(sol_dfl)$"sim_mats_list" <- sml
    attributes(sol_dfl)$"snf_config" <- sc
    sol_dfl <- validate_solutions_df(sol_dfl)
    sol_df <- new_solutions_df(sol_dfl)
    return(sol_df)
}

#' Validator for `solutions_df` class object
#'
#' @inheritParams solutions_df
#' @return RETURN
#' @export
validate_solutions_df <- function(sol_dfl) {
    class(sol_dfl)  <- setdiff(class(sol_dfl), "solutions_df")
    if (!identical(colnames(sol_dfl)[1:2], c("solution", "nclust"))) {
        metasnf_error(
            "First two columns of `solutions_df` must be \"solution\" and",
            " \"nclust\"."
        )
    }
    return(sol_dfl)
}

#' Constructor for `solutions_df` class object
#'
#' @inheritParams solutions_df 
#' @return RETURN
#' @export
new_solutions_df <- function(sol_dfl) {
    sol_df <- structure(sol_dfl, class = c("solutions_df", "data.frame"))    
    return(sol_df)
}

#' Extend a solutions data frame to include outcome evaluations
#'
#' @param sol_df Result of `batch_snf` storing cluster solutions and
#'  the settings that were used to generate them.
#' @param target_dl A data list with features to calculate p-values for.
#'  Features in the target list will be included during p-value summary
#'  measure calculations.
#' @param dl A data list with features to calcualte p-values for, but
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
    # Check to see if the dl and sol_df have matching subjects
    ###########################################################################
    solution_subs <- uids(sol_df)
    dl_subs <- uids(dl)
    if (!identical(solution_subs, dl_subs)) {
        metasnf_error(
            "Subjects in data list/target list do not match those in",
            " sol_df."
        )
    }
    ###########################################################################
    # Calculate vector of all feature names
    ###########################################################################
    fts <- as.character(unlist(sapply(dl, function(x) colnames(x$"data")[-1])))
    n_fts <- length(fts)
    ###########################################################################
    # Calculate vector of all feature types
    ###########################################################################
    feature_types <- sapply(dl, function(x) rep(x$"type", n_fts))
    ###########################################################################
    # Construct base of extended solutions data frame by adding columns for
    # p-values of all fts
    ###########################################################################
    # Specifying the dataframe structure avoids tibble-related errors
    ext_sol_df <- data.frame(matrix(NA, nrow = nrow(sol_df), ncol = 1 + n_fts))
    colnames(ext_sol_df) <- c("solution", paste0(fts, "_pval"))
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
            clustered_subs <- t(sol_df[i, ])
            for (j in seq_along(fts)) {
                current_component_df <- merged_df[, c(1, j + 1)]
                current_ft <- colnames(current_component_df)[2]
                suppressWarnings({
                    pval <- calc_assoc_pval(
                        clustered_subs[, 2],
                        current_component_df[, 2],
                        "categorical",
                        feature_types[j],
                        cat_test = cat_test
                    )
                })
                target_col <- which(
                    paste0(current_ft, "_pval") == colnames(ext_sol_df)
                )
                ext_sol_df[[target_col]][i] <- pval
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
        ext_sol_df_rows <- future.apply::future_lapply(
            seq_len(nrow(ext_sol_df)),
            function(i) {
                clustered_subs <- t(sol_df[i, ])
                for (j in seq_along(fts)) {
                    current_component_df <- merged_df[, c(1, j + 1)]
                    current_ft <- colnames(current_component_df)[2]
                    evaluation_df <- dplyr::inner_join(
                        clustered_subs,
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
                        paste0(current_ft, "_pval") == colnames(ext_sol_df)
                    )
                    ext_sol_df[[target_col]][i] <- pval
                }
                return(ext_sol_df[i, ])
            }
        )
        future::plan(future::sequential)
        ext_sol_df <- do.call("rbind", ext_sol_df_rows)
    }
    ext_sol_df$"solution" <- sol_df$"solution"
    ext_sol_df <- ext_sol_df |> dplyr::select(
        "solution",
        dplyr::everything()
    )
    ###########################################################################
    # If min_pval is assigned, use to replace any smaller p-value
    ###########################################################################
    if (!is.null(min_pval)) {
        ext_sol_df <- ext_sol_df |>
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
        target_ext_sol_df <- dplyr::select(
            ext_sol_df,
            "solution",
            dplyr::all_of(target_fts)
        )
        target_ext_sol_df <- summarize_pvals(target_ext_sol_df)
        target_ext_sol_df <- dplyr::select(target_ext_sol_df, -"solution")
        ext_sol_df <- dplyr::select(ext_sol_df, -dplyr::all_of(target_fts))
        ext_sol_df <- cbind(ext_sol_df, target_ext_sol_df)
    }
    ext_sol_df <- numcol_to_numeric(ext_sol_df)
    ext_sol_df$"solution" <- as.integer(ext_sol_df$"solution")
    ext_sol_df <- dplyr::inner_join(sol_df, ext_sol_df, by = "solution")
    if (!is.null(target_dl)) {
        ext_sol_df <- dplyr::select(
            ext_sol_df,
            "solution",
            "nclust",
            "mc",
            "min_pval",
            "mean_pval",
            "max_pval",
            dplyr::everything())
    }
    attributes(ext_sol_df)$"features" <- fts
    attributes(ext_sol_df)$"snf_config" <- attributes(sol_df)$"snf_config"
    class(ext_sol_df) <- c("ext_solutions_df", "data.frame")
    if (!is.null(target_dl)) {
        attributes(ext_sol_df)$"summary_features" <- features(target_dl)
    }
    return(ext_sol_df)
}

#' Get p-values from an extended solutions data frame
#'
#' This function can be used to neatly format the p-values associated with an
#' extended solutions data frame. It can also calculate the negative logs of those
#' p-values to make it easier to interpret large-scale differences.
#'
#' @param ext_sol_df The output of `extend_solutions`. A
#' dataframe that contains at least one p-value column ending in "_pval".
#'
#' @param negative_log If TRUE, will replace p-values with negative log
#' p-values.
#'
#' @param keep_summaries If FALSE, will remove the mean, min, and max p-value.
#'
#' @return A "data.frame" class object Of only the p-value related columns
#' of the provided ext_sol_df.
#'
#' @export
get_pvals <- function(ext_sol_df,
                      negative_log = FALSE,
                      keep_summaries = TRUE) {
    # Select p-value columns and convert to numeric
    pval_df <- ext_sol_df |>
        dplyr::select(
            "solution",
            dplyr::ends_with("_pval")
        ) |>
        data.frame() |>
        metasnf::numcol_to_numeric()
    # Convert p-values to negative log p-values if requested
    if (negative_log) {
        # Negative log conversions
        neg_log_pval_df <- -log(pval_df)
        neg_log_pval_df$"solution" <- pval_df$"solution"
        return(neg_log_pval_df)
    }
    if (!keep_summaries) {
        pval_df <- dplyr::select(
            pval_df,
            -"mean_pval",
            -"min_pval",
            -"max_pval"
        )
    }
    return(pval_df)
}

#' Summarize p-value columns of an extended solutions data frame
#'
#' @param ext_sol_df Result of `extend_solutions`
#'
#' @return The provided extended solutions data frame along with columns for
#' the min, mean, and maximum across p-values for each row.
#'
#' @export
summarize_pvals <- function(ext_sol_df) {
    # Restrict to just p-value columns
    pval_cols <- dplyr::select(
        ext_sol_df,
        dplyr::ends_with("_pval")
    ) |>
        numcol_to_numeric()
    # Calculate summary statistics
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
    max_pvals <- apply(
        pval_cols,
        1,
        FUN = function(x) {
            max(x, na.rm = TRUE)
        }
    )
    # Attach summary statistics to the solutions data frame
    ext_sol_df$"min_pval" <- min_pvals
    ext_sol_df$"mean_pval" <- mean_pvals
    ext_sol_df$"max_pval" <- max_pvals
    return(ext_sol_df)
}


#' Get minimum p-value
#'
#' Given an solutions data frame row containing evaluated p-values, returns min.
#'
#' @param sol_df_row row of sol_df object
#'
#' @return min_pval minimum p-value
#'
#' @export
get_min_pval <- function(sol_df_row) {
    min_pval <- sol_df_row |>
        dplyr::mutate(
            dplyr::across(dplyr::ends_with("_pval"), ~ as.numeric(.))
        ) |>
        dplyr::select(dplyr::ends_with("_pval")) |>
        min()
    return(min_pval)
}

#' Get mean p-value
#'
#' Given an solutions data frame row containing evaluated p-values, returns mean.
#'
#' @param sol_df_row row of sol_df object
#'
#' @return mean_pval mean p-value
#'
#' @export
get_mean_pval <- function(sol_df_row) {
    mean_pval <- sol_df_row |>
        dplyr::mutate(
            dplyr::across(dplyr::ends_with("_pval"), ~ as.numeric(.))
        ) |>
        dplyr::select(dplyr::ends_with("_pval")) |>
        rowMeans()
    return(mean_pval)
}

#' Ordinal regression p-value
#'
#' Returns the overall p-value of an ordinal regression on a categorical
#' predictor and response vetors. If the ordinal response
#'
#' @param predictor A categorical or numeric feature.
#'
#' @param response A numeric feature.
#'
#' @return pval A p-value (class "numeric").
#'
#' @export
ord_reg_pval <- function(predictor, response) {
    # If there are only 2 tiers to the ordinal scale, just use linear model
    num_classes <- length(unique(response))
    if (num_classes == 2) {
        return(linear_model_pval(predictor, response))
    }
    # Otherwise, run regular ordinal regression
    response <- as.ordered(response)
    null_model <- MASS::polr(response ~ 1)
    result <- tryCatch(
        {
            full_model <- MASS::polr(response ~ predictor)
            pval <- stats::anova(null_model, full_model)$"Pr(Chi)"[2]
            return(pval)
        },
        error = function(e) {
            if (grepl("suitable starting values failed", e$message)) {
                metasnf_warning(
                    "Ordinal regression failed due to MASS:polr error:",
                    " 'attempt to find suitable starting values failed'. p-value",
                    " calculated by linear regression instead."
                )
                return(linear_model_pval(predictor, response))
            } else {
                metasnf_warning(
                    "Error during ordinal regression: ", e$message
                )
                return(NA)
            }
        }
    )
}

#' Chi-squared test p-value (generic)
#'
#' Return p-value for chi-squared test for any two features
#'
#' @param cat_var1 A categorical feature.
#' @param cat_var2 A categorical feature.
#'
#' @return pval A p-value (class "numeric").
#'
#' @export
chi_squared_pval <- function(cat_var1, cat_var2) {
    cat_var1 <- factor(cat_var1)
    cat_var2 <- factor(cat_var2)
    suppressWarnings(
        model <- stats::chisq.test(cat_var1, cat_var2, correct = FALSE)
    )
    pval <- model$"p.value"
    attributes(pval) <- NULL
    return(pval)
}

#' Fisher exact test p-value
#'
#' Return p-value for Fisher exact test for any two features
#'
#' @param cat_var1 A categorical feature.
#' @param cat_var2 A categorical feature.
#'
#' @return pval A p-value (class "numeric").
#'
#' @export
fisher_exact_pval <- function(cat_var1, cat_var2) {
    cat_var1 <- factor(cat_var1)
    cat_var2 <- factor(cat_var2)
    model <- stats::fisher.test(cat_var1, cat_var2, workspace = 2e7)
    pval <- model$"p.value"
    attributes(pval) <- NULL
    return(pval)
}

#' Linear model p-value (generic)
#'
#' Return p-value of F-test for a linear model of any two features
#'
#' @param predictor A categorical or numeric feature.
#' @param response A numeric feature.
#'
#' @return pval A p-value (class "numeric").
#'
#' @export
linear_model_pval <- function(predictor, response) {
    model <- stats::lm(response ~ predictor)
    fstat <- summary(model)$"fstatistic"
    pval <- stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
    attributes(pval) <- NULL
    return(pval)
}

#' Calculate p-values based on feature vectors and their types
#'
#' @param var1 A single vector containing a feature.
#' @param var2 A single vector containing a feature.
#' @param type1 The type of var1 (continuous, discrete, ordinal, categorical).
#' @param type2 The type of var2 (continuous, discrete, ordinal, categorical).
#' @param cat_test String indicating which statistical test will be used to
#' associate cluster with a categorical feature. Options are "chi_squared" for
#' the Chi-squared test and "fisher_exact" for Fisher's exact test.
#'
#' @return pval A p-value from a statistical test based on the provided types.
#'  Currently, this will either be the F-test p-value from a linear model
#'  if at least one feature is non-categorical, or the chi-squared test
#'  p-value if both features are categorical.
#'
#' @export
calc_assoc_pval <- function(var1,
                            var2,
                            type1,
                            type2,
                            cat_test = "chi_squared") {
    ###########################################################################
    # Strip any list structure from the features
    ###########################################################################
    var1 <- unlist(var1)
    var2 <- unlist(var2)
    ###########################################################################
    # Double categorical features
    ###########################################################################
    if (type1 == "categorical" && type2 == "categorical") {
        cat_test_fns <- list(
            "chi_squared" = chi_squared_pval,
            "fisher_exact" = fisher_exact_pval
        )
        cat_test_fn <- cat_test_fns[[cat_test]]
        pval <- cat_test_fn(var1, var2)
        return(pval)
    }
    ###########################################################################
    # Any ordinal feature
    ###########################################################################
    if (type1 == "ordinal") {
        if (type2 == "categorical") {
            pval <- ord_reg_pval(
                predictor = factor(var2),
                response = var1
            )
            return(pval)
        } else {
            pval <- ord_reg_pval(
                predictor = var2,
                response = var1
            )
            return(pval)
        }
    }
    if (type2 == "ordinal") {
        if (type1 == "categorical") {
            pval <- ord_reg_pval(
                predictor = factor(var1),
                response = var2
            )
            return(pval)
        } else {
            pval <- ord_reg_pval(
                predictor = var1,
                response = var2
            )
            return(pval)
        }
    }
    ###########################################################################
    # One categorical feature, one numeric feature
    ###########################################################################
    if (type1 == "categorical") {
        pval <- linear_model_pval(
            predictor = factor(var1),
            response = var2
        )
        return(pval)
    }
    if (type2 == "categorical") {
        pval <- linear_model_pval(
            predictor = factor(var2),
            response = var1
        )
        return(pval)
    }
    ###########################################################################
    # Two numeric features
    ###########################################################################
    pval <- linear_model_pval(var1, var2)
    return(pval)
}

#' Calculate p-values for all pairwise associations of features in a data list
#'
#' @param dl A nested list of input data from `data_list()`.
#'
#' @param verbose If TRUE, output progress to the console.
#'
#' @param cat_test String indicating which statistical test will be used to
#' associate cluster with a categorical feature. Options are "chi_squared" for
#' the Chi-squared test and "fisher_exact" for Fisher's exact test.
#'
#' @return A "matrix" class object containing pairwise association p-values
#' between the features in the provided data list.
#'
#' @export
calc_assoc_pval_matrix <- function(dl,
                                   verbose = FALSE,
                                   cat_test = "chi_squared") {
    ###########################################################################
    # Build a single data.frame that contains all data
    ###########################################################################
    merged_df <- as.data.frame(dl)
    merged_df <- merged_df[, colnames(merged_df) != "uid"]
    ###########################################################################
    # Build data.frame containing the types of features in merged_df
    ###########################################################################
    types <- dl |>
        lapply(
            function(x) {
                rep(x$"type", ncol(x$"data") - 1)
            }
        ) |>
        unlist()
    domains <- dl |>
        lapply(
            function(x) {
                rep(x$"domain", ncol(x$"data") - 1)
            }
        ) |>
        unlist()
    var_names <- colnames(merged_df[, colnames(merged_df) != "uid"])
    metadata <- data.frame(
        name = var_names,
        type = types,
        domain = domains
    )
    ###########################################################################
    # Ensure that 'mixed' data type is not being used
    ###########################################################################
    dl_summary <- summary(dl)
    if (any(dl_summary$"type" == "mixed")) {
        metasnf_warning(
            "When using the 'mixed' data type in the 'calculate_associations'",
            " function, any data that can be converted to numeric format will",
            " be treated as continuous and all others will be treated as",
            " categorical. If you do not want this behaviour, please",
            " restructure your input data to only use the following types:",
            " continuous, discrete, ordinal, or categorical."
        )
        merged_df <- numcol_to_numeric(merged_df)
        classes <- as.vector(sapply(merged_df, class))
        metadata$"type"[classes == "numeric"] <- "continuous"
        metadata$"type"[classes != "numeric"] <- "categorical"
    }
    ###########################################################################
    # Loop through all pairs of features
    ###########################################################################
    pairwise_indices <- utils::combn(ncol(merged_df), 2)
    association_matrix <- matrix(
        ncol = ncol(merged_df),
        nrow = ncol(merged_df),
        0
    )
    colnames(association_matrix) <- colnames(merged_df)
    rownames(association_matrix) <- colnames(merged_df)
    for (col in seq_len(ncol(pairwise_indices))) {
        ## The positions of the two features in the merged dataframe
        ind1 <- pairwise_indices[1, col]
        ind2 <- pairwise_indices[2, col]
        # The actual features
        var1 <- merged_df[, ind1]
        var2 <- merged_df[, ind2]
        # The names of the features
        var1_name <- colnames(merged_df)[ind1]
        var2_name <- colnames(merged_df)[ind2]
        # Types of the features
        var1_type <- metadata[metadata$"name" == var1_name, "type"]
        var2_type <- metadata[metadata$"name" == var2_name, "type"]
        # Output current comparison if user specified verbose = TRUE
        if (verbose) {
            cat(
                "Calculating ", var1_name, " (", var1_type, ") vs.",
                " ", var2_name, " (", var2_type, ")...\n",
                sep = ""
            )
        }
        #######################################################################
        # Calculate p-values
        #######################################################################
        pval <- calc_assoc_pval(
            var1,
            var2,
            var1_type,
            var2_type,
            cat_test
        )
        association_matrix[ind1, ind2] <- pval
        association_matrix[ind2, ind1] <- pval
    }
    return(association_matrix)
}
