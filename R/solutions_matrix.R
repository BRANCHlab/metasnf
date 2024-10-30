#' Extend an solutions matrix to include outcome evaluations
#'
#' @param solutions_matrix Result of `batch_snf` storing cluster solutions and
#' the settings that were used to generate them.
#'
#' @param data_list A data_list with features to calcualte p-values for, but
#' that should not be incorporated into p-value summary measure columns (i.e.,
#' min/mean/max p-value columns).
#'
#' @param target_list A data_list with features to calculate p-values for.
#' Features in the target list will be included during p-value summary
#' measure calculations.
#'
#' @param cat_test String indicating which statistical test will be used to
#' associate cluster with a categorical feature. Options are "chi_squared" for
#' the Chi-squared test and "fisher_exact" for Fisher's exact test.
#'
#' @param calculate_summaries If TRUE, the function will calculate the minimum
#' and mean p-values for each row of the solutions matrix.
#'
#' @param min_pval If assigned a value, any p-value less than this will be
#' replaced with this value.
#'
#' @param processes The number of processes to use for parallelization.
#' Progress is only reported for sequential processing (processes = 1).
#'
#' @return extended_solutions_matrix an extended solutions matrix that contains
#'  p-value columns for each outcome in the provided target_list
#'
#' @param verbose If TRUE, print progress to console.
#'
#' @export
extend_solutions <- function(solutions_matrix,
                             target_list = NULL,
                             data_list = NULL,
                             cat_test = "chi_squared",
                             calculate_summaries = TRUE,
                             min_pval = 1e-10,
                             processes = 1,
                             verbose = FALSE) {
    ###########################################################################
    # Remove nclust = 1 solutions
    ###########################################################################
    single_cluster_solutions <- apply(
        get_cluster_solutions(solutions_matrix)[, -1, drop = FALSE],
        2,
        function(x) length(unique(x)) == 1
    ) |>
        as.logical() |>
        which()
    if (length(single_cluster_solutions) > 0) {
        warning(
            "Single-cluster solution rows removed: ",
            single_cluster_solutions
        )
        solutions_matrix <- solutions_matrix[-c(single_cluster_solutions), ]
    }
    ###########################################################################
    # If data_list and target_list exist, merge them as data_list.
    ###########################################################################
    data_list <- c(data_list, target_list)
    ###########################################################################
    # If target_list not given but calculate_summaries is TRUE, give a warning.
    ###########################################################################
    if (is.null(target_list) && calculate_summaries) {
        warning(
            "Calculate summaries only applies to target_list features, but",
            " target_list parameter was not specified."
        )
    }
    ###########################################################################
    # Check to see if the data_list and solutions_matrix have matching subjects
    ###########################################################################
    solution_subs <- colnames(subs(solutions_matrix))[-1]
    target_subs <- target_list[[1]]$"data"$"subjectkey"
    if (!identical(solution_subs, target_subs)) {
        stop(
            "Subjects in data_list/target_list do not match those in",
            " solutions_matrix."
        )
    }
    ###########################################################################
    # Calculate vector of all feature names
    ###########################################################################
    features <- data_list |>
        lapply(
            function(x) {
                x$"data" |>
                    dplyr::select(-"subjectkey") |>
                    colnames()
            }
        ) |>
        unlist()
    ###########################################################################
    # Calculate vector of all feature types
    ###########################################################################
    feature_types <- data_list |>
        lapply(
            function(x) {
                n_features <- ncol(x$"data") - 1
                outcome_type <- rep(x$"type", n_features)
                return(outcome_type)
            }
        ) |>
        unlist()
    ###########################################################################
    # Construct base of extended solutions matrix by adding columns for
    # p-values of all features
    ###########################################################################
    # Specifying the dataframe structure avoids tibble-related errors
    esm <- solutions_matrix |>
        data.frame() |>
        add_columns(
            paste0(features, "_pval"),
            fill = NA
        )
    ###########################################################################
    # Single DF to contain all features to calculate p-values for
    ###########################################################################
    merged_df <- data_list |>
        lapply(
            function(x) {
                x[[1]]
            }
        ) |>
        merge_df_list()
    ###########################################################################
    # Sequential extension
    ###########################################################################
    if (processes == 1) {
        # Iterate across rows of the solutions matrix
        for (i in seq_len(nrow(esm))) {
            if (verbose) {
                print(paste0("Processing row ", i, " of ", nrow(esm)))
            }
            clustered_subs <- get_cluster_df(esm[i, ])
            for (j in seq_along(features)) {
                current_component_df <- merged_df[, c(1, j + 1)]
                current_feature <- colnames(current_component_df)[2]
                evaluation_df <- dplyr::inner_join(
                    clustered_subs,
                    current_component_df,
                    by = "subjectkey"
                )
                suppressWarnings({
                    pval <- calc_assoc_pval(
                        evaluation_df[, "cluster"],
                        evaluation_df[, features[j]],
                        "categorical",
                        feature_types[j],
                        cat_test = cat_test
                    )
                })
                target_col <- which(
                    paste0(current_feature, "_pval") == colnames(esm)
                )
                esm[i, target_col] <- pval
            }
        }
    } else {
        #######################################################################
        # Parallel extension
        #######################################################################
        max_cores <- future::availableCores()
        if (processes == "max") {
            processes <- max_cores
        } else if (processes > max_cores) {
            warning(
                paste0(
                    "Requested processes exceed available cores.",
                    " Defaulting to the max available (", max_cores, ")."
                )
            )
            processes <- max_cores
        }
        # Iterate across rows of the solutions matrix
        future::plan(future::multisession, workers = processes)
        esm_rows <- future.apply::future_lapply(
            seq_len(nrow(esm)),
            function(i) {
                clustered_subs <- get_cluster_df(esm[i, ])
                for (j in seq_along(features)) {
                    current_component_df <- merged_df[, c(1, j + 1)]
                    current_feature <- colnames(current_component_df)[2]
                    evaluation_df <- dplyr::inner_join(
                        clustered_subs,
                        current_component_df,
                        by = "subjectkey"
                    )
                    suppressWarnings({
                        pval <- calc_assoc_pval(
                            evaluation_df[, "cluster"],
                            evaluation_df[, features[j]],
                            "categorical",
                            feature_types[j],
                            cat_test = cat_test
                        )
                    })
                    target_col <- which(
                        paste0(current_feature, "_pval") == colnames(esm)
                    )
                    esm[i, target_col] <- pval
                }
                return(esm[i, ])
            }
        )
        future::plan(future::sequential)
        esm <- do.call("rbind", esm_rows)
    }
    ###########################################################################
    # If min_pval is assigned, use to replace any smaller p-value
    ###########################################################################
    if (!is.null(min_pval)) {
        esm <- esm |>
            numcol_to_numeric() |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::ends_with("_pval"),
                    ~ ifelse(. < min_pval, min_pval, .)
                )
            )
    }
    if (calculate_summaries) {
        #######################################################################
        # Identify features present in target_list
        #######################################################################
        target_features <- dl_variable_summary(target_list)$"name"
        target_features <- paste0(target_features, "_pval")
        target_esm <- dplyr::select(
            esm,
            "row_id",
            dplyr::all_of(target_features)
        )
        target_esm <- summarize_pvals(target_esm)
        target_esm <- dplyr::select(target_esm, -"row_id")
        esm <- dplyr::select(esm, -dplyr::all_of(target_features))
        esm <- cbind(esm, target_esm)
    }
    esm <- numcol_to_numeric(esm)
    return(esm)
}

#' Get p-values from an extended solutions matrix
#'
#' This function can be used to neatly format the p-values associated with an
#' extended solutions matrix. It can also calculate the negative logs of those
#' p-values to make it easier to interpret large-scale differences.
#'
#' @param extended_solutions_matrix The output of `extend_solutions`. A
#' dataframe that contains at least one p-value column ending in "_pval".
#'
#' @param negative_log If TRUE, will replace p-values with negative log
#' p-values.
#'
#' @param keep_summaries If FALSE, will remove the mean, min, and max p-value.
#'
#' @return A "data.frame" class object Of only the p-value related columns
#' of the provided extended_solutions_matrix.
#'
#' @export
get_pvals <- function(extended_solutions_matrix,
                      negative_log = FALSE,
                      keep_summaries = TRUE) {
    # Select p-value columns and convert to numeric
    pval_df <- extended_solutions_matrix |>
        dplyr::select(
            "row_id",
            dplyr::ends_with("_pval")
        ) |>
        data.frame() |>
        metasnf::numcol_to_numeric()
    # Convert p-values to negative log p-values if requested
    if (negative_log) {
        # Negative log conversions
        neg_log_pval_df <- -log(pval_df)
        neg_log_pval_df$"row_id" <- pval_df$"row_id"
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

#' Summarize p-value columns of an extended solutions matrix
#'
#' @param extended_solutions_matrix Result of `extend_solutions`
#'
#' @return The provided extended solutions matrix along with columns for
#' the min, mean, and maximum across p-values for each row.
#'
#' @export
summarize_pvals <- function(extended_solutions_matrix) {
    # Restrict to just p-value columns
    pval_cols <- dplyr::select(
        extended_solutions_matrix,
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
    # Attach summary statistics to the solutions matrix
    extended_solutions_matrix$"min_pval" <- min_pvals
    extended_solutions_matrix$"mean_pval" <- mean_pvals
    extended_solutions_matrix$"max_pval" <- max_pvals
    return(extended_solutions_matrix)
}


#' Get minimum p-value
#'
#' Given an solutions matrix row containing evaluated p-values, returns min.
#'
#' @param solutions_matrix_row row of solutions_matrix object
#'
#' @return min_pval minimum p-value
#'
#' @export
get_min_pval <- function(solutions_matrix_row) {
    min_pval <- solutions_matrix_row |>
        dplyr::mutate(
            dplyr::across(dplyr::ends_with("_pval"), ~ as.numeric(.))
        ) |>
        dplyr::select(dplyr::ends_with("_pval")) |>
        min()
    return(min_pval)
}

#' Get mean p-value
#'
#' Given an solutions matrix row containing evaluated p-values, returns mean.
#'
#' @param solutions_matrix_row row of solutions_matrix object
#'
#' @return mean_pval mean p-value
#'
#' @export
get_mean_pval <- function(solutions_matrix_row) {
    mean_pval <- solutions_matrix_row |>
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
    result <- tryCatch({
        full_model <- MASS::polr(response ~ predictor)
        pval <- stats::anova(null_model, full_model)$"Pr(Chi)"[2]
        return(pval)
    },
    error = function(e) {
        if (grepl("suitable starting values failed", e$message)) {
            warning(
                "Ordinal regression failed due to MASS:polr error:",
                " 'attempt to find suitable starting values failed'. p-value",
                " calculated by linear regression instead."
            )
            return(linear_model_pval(predictor, response))
        } else {
            warning(paste("Error during ordinal regression: ", e$message))
            return(NA)
        }
    })
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

#' Calculate p-values for all pairwise associations of features in a data_list
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @param verbose If TRUE, prints new line everytime a p-value is being
#'  calculated.
#'
#' @param cat_test String indicating which statistical test will be used to
#' associate cluster with a categorical feature. Options are "chi_squared" for
#' the Chi-squared test and "fisher_exact" for Fisher's exact test.
#'
#' @return A "matrix" class object containing pairwise association p-values
#' between the features in the provided data list.
#'
#' @export
calc_assoc_pval_matrix <- function(data_list,
                                   verbose = FALSE,
                                   cat_test = "chi_squared") {
    ###########################################################################
    # Build a single data.frame that contains all data
    ###########################################################################
    merged_df <- collapse_dl(data_list)
    merged_df <- merged_df[, colnames(merged_df) != "subjectkey"]
    ###########################################################################
    # Build data.frame containing the types of features in merged_df
    ###########################################################################
    types <- data_list |>
        lapply(
            function(x) {
                rep(x$"type", ncol(x$"data") - 1)
            }
        ) |>
        unlist()
    domains <- data_list |>
        lapply(
            function(x) {
                rep(x$"domain", ncol(x$"data") - 1)
            }
        ) |>
        unlist()
    var_names <- colnames(merged_df[, colnames(merged_df) != "subjectkey"])
    metadata <- data.frame(
        name = var_names,
        type = types,
        domain = domains
    )
    ###########################################################################
    # Ensure that 'mixed' data type is not being used
    ###########################################################################
    dl_summary <- summarize_dl(data_list)
    if (any(dl_summary$"type" == "mixed")) {
        warning(
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
            print(
                paste0(
                    "Calculating ", var1_name, " (", var1_type, ") vs.",
                    " ", var2_name, " (", var2_type, ")..."
                ),
                quote = FALSE
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
