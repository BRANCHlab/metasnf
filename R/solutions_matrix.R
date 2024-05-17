#' Extend an solutions matrix to include outcome evaluations
#'
#' @param solutions_matrix A solutions_matrix.
#'
#' @param target_list A data_list with variables to calculate p-values for.
#'
#' @param cat_test String indicating which statistical test will be used to
#' associate cluster with a categorical variable. Options are "chi_squared" for
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
#' @export
extend_solutions <- function(solutions_matrix,
                             target_list,
                             cat_test = "chi_squared",
                             calculate_summaries = TRUE,
                             min_pval = NULL,
                             processes = 1) {
    ###########################################################################
    # Calculate vector of all feature names
    ###########################################################################
    features <- target_list |>
        lapply(
            function(x) {
                colnames(x[[1]])[-1]
            }
        ) |>
        unlist()
    ###########################################################################
    # Calculate vector of all feature types
    ###########################################################################
    feature_types <- target_list |>
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
    merged_df <- target_list |>
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
            print(paste0("Processing row ", i, " of ", nrow(esm)))
            clustered_subs <- get_cluster_df(esm[i, ])
            for (j in seq_along(features)) {
                current_outcome_component <- merged_df[, c(1, j + 1)]
                current_outcome_name <- colnames(current_outcome_component)[2]
                suppressWarnings(
                    pval <- get_cluster_pval(
                        clustered_subs,
                        current_outcome_component,
                        feature_types[j],
                        features[j],
                        cat_test = cat_test
                    )
                )
                target_col <- which(
                    paste0(current_outcome_name, "_pval") == colnames(esm)
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
            print(
                paste0(
                    "Requested processes exceed available cores.",
                    " Defaulting to the max avaiilable (", max_cores, ")."
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
                    current_outcome_component <- merged_df[, c(1, j + 1)]
                    current_outcome_name <-
                        colnames(current_outcome_component)[2]
                    suppressWarnings(
                        pval <- get_cluster_pval(
                            clustered_subs,
                            current_outcome_component,
                            feature_types[j],
                            features[j],
                            cat_test = cat_test
                        )
                    )
                    target_col <- which(
                        paste0(current_outcome_name, "_pval") == colnames(esm)
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
        esm <- summarize_pvals(esm)
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
#' @param negative_log If TRUE, will replace p-values with negative log
#' p-values.
#'
#' @export
get_pvals <- function(extended_solutions_matrix,
                      negative_log = FALSE) {
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
    return(pval_df)
}

#' Summarize p-value columns of an extended solutions matrix
#'
#' @param extended_solutions_matrix Result of `extend_solutions`
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

#' Get p-value
#'
#' Depending on outcome measure, perform ordinal regression or linear regression
#'  and return p-value as a benchmark measure of how well-separated clusters
#'  are by the outcome measure.
#'
#' @param assigned_subs dataframe of subjects who were assigned to a cluster
#' and the cluster they were assigned to.
#' @param outcome_df dataframe containing subjectkey and outcome feautre column
#' @param outcome_type string indicating the outcome type (numeric or ordinal)
#' @param outcome_name string indicating the name of the feature
#' @param cat_test String indicating which statistical test will be used to
#' associate cluster with a categorical variable. Options are "chi_squared" for
#' the Chi-squared test and "fisher_exact" for Fisher's exact test.
#'
#' @return pval the smallest p-value of interest
#'
#' @export
get_cluster_pval <- function(assigned_subs,
                             outcome_df,
                             outcome_type,
                             outcome_name,
                             cat_test = "chi_squared") {
    # Dataframe containing cluster membership and outcome variable as cols
    merged_df <- dplyr::inner_join(
        assigned_subs,
        outcome_df,
        by = "subjectkey"
    )
    if (outcome_type == "ordinal") {
        pval <- ord_reg_pval(
            predictor = factor(merged_df$"cluster"),
            response = merged_df[, outcome_name]
        )
    } else if (outcome_type %in% c("numeric", "discrete", "continuous")) {
        pval <- linear_model_pval(
            predictor = factor(merged_df$"cluster"),
            response = merged_df[, outcome_name]
        )
    } else if (outcome_type == "categorical") {
        if (cat_test == "chi_squared") {
            pval <- chi_squared_pval(
                merged_df$"cluster",
                merged_df[, outcome_name]
            )
        } else if (cat_test == "fisher_exact") {
            pval <- fisher_exact_pval(
                merged_df$"cluster",
                merged_df[, outcome_name]
            )
        }
    } else {
        stop(
            "Unsupported outcome type: ", outcome_type,
            ". Accepted types for now are numeric (continuous, discrete,",
            " ordinal) and categorical."
        )
    }
    return(pval)
}

#' Ordinal regression p-value
#'
#' Returns the overall p-value of an ordinal regression on a categorical
#' predictor and response vetors. If the ordinal response
#'
#' @param predictor A categorical or numeric variable.
#' @param response A numeric variable.
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
    full_model <- MASS::polr(response ~ predictor)
    pval <- stats::anova(null_model, full_model)$"Pr(Chi)"[2]
    return(pval)
}

#' Chi-squared test p-value (generic)
#'
#' Return p-value for chi-squared test for any two variables
#'
#' @param cat_var1 A categorical variable.
#' @param cat_var2 A categorical variable.
#'
#' @return pval A p-value.
#'
#' @export
chi_squared_pval <- function(cat_var1, cat_var2) {
    cat_var1 <- factor(cat_var1)
    cat_var2 <- factor(cat_var2)
    model <- stats::chisq.test(cat_var1, cat_var2, correct = FALSE)
    pval <- model$"p.value"
    attributes(pval) <- NULL
    return(pval)
}

#' Fisher exact test p-value
#'
#' Return p-value for Fisher exact test for any two variables
#'
#' @param cat_var1 A categorical variable.
#' @param cat_var2 A categorical variable.
#'
#' @return pval A p-value.
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
#' Return p-value of F-test for a linear model of any two variables
#'
#' @param predictor A categorical or numeric variable.
#' @param response A numeric variable.
#'
#' @return pval A p-value.
#'
#' @export
linear_model_pval <- function(predictor, response) {
    model <- stats::lm(response ~ predictor)
    fstat <- summary(model)$"fstatistic"
    pval <- stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
    attributes(pval) <- NULL
    return(pval)
}

#' Calculate p-values based on variable vectors and their types
#'
#' @param var1 A single vector containing a variable.
#' @param var2 A single vector containing a variable.
#' @param type1 The type of var1 (continuous, discrete, ordinal, categorical).
#' @param type2 The type of var2 (continuous, discrete, ordinal, categorical).
#' @param cat_test String indicating which statistical test will be used to
#' associate cluster with a categorical variable. Options are "chi_squared" for
#' the Chi-squared test and "fisher_exact" for Fisher's exact test.
#'
#' @return pval A p-value from a statistical test based on the provided types.
#'  Currently, this will either be the F-test p-value from a linear model
#'  if at least one variable is non-categorical, or the chi-squared test
#'  p-value if both variables are categorical.
#'
#' @export
calculate_association_pval <- function(var1,
                                       var2,
                                       type1,
                                       type2,
                                       cat_test = "chi_squared") {
    types <- c(type1, type2)
    numeric_vars <- c("continuous", "discrete", "ordinal")
    if (all(types %in% numeric_vars)) {
        # numeric vs. numeric
        num_var1 <- as.numeric(unlist(var1))
        num_var2 <- as.numeric(unlist(var2))
        pval <- linear_model_pval(num_var1, num_var2)
    } else if (all(types %in% "categorical")) {
        # categorical vs. categorical
        cat_var1 <- factor(unlist(var1))
        cat_var2 <- factor(unlist(var2))
        if (cat_test == "chi_squared") {
            pval <- chi_squared_pval(cat_var1, cat_var2)
        } else if (cat_test == "fisher_exact") {
            pval <- fisher_exact_pval(cat_var1, cat_var2)
        }
    } else {
        # numeric vs. categorical
        if (which(types %in% numeric_vars) == 1) {
            num_var <- as.numeric(unlist(var1))
            cat_var <- factor(unlist(var2))
        } else {
            num_var <- as.numeric(unlist(var2))
            cat_var <- factor(unlist(var1))
        }
        pval <- linear_model_pval(predictor = cat_var, response = num_var)
    }
    return(pval)
}

#' Calculate p-values for pairwise associations of variables in a data_list
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#' @param verbose If TRUE, prints new line everytime a p-value is being
#'  calculated.
#' @param key_association If a variable is named, returns a dataframe of
#'  p-values relative to that variable rather than all pairwise p-values.
#' @param drop_self If key_association is specified and drop_self is TRUE,
#'  removes the p-value row of the key_association variable with itself (0).
#' @param cat_test String indicating which statistical test will be used to
#' associate cluster with a categorical variable. Options are "chi_squared" for
#' the Chi-squared test and "fisher_exact" for Fisher's exact test.
#'
#' @export
calculate_associations <- function(data_list,
                                   verbose = FALSE,
                                   key_association = NULL,
                                   drop_self = TRUE,
                                   cat_test = "chi_squared") {
    ###########################################################################
    # Build a single data.frame that contains all data
    ###########################################################################
    merged_df <- collapse_dl(data_list)
    merged_df <- merged_df[, colnames(merged_df) != "subjectkey"]
    ###########################################################################
    # Build data.frame containing the types of variables in merged_df
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
    # Loop through all pairs of variables
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
        ## The positions of the two variables in the merged dataframe
        ind1 <- pairwise_indices[1, col]
        ind2 <- pairwise_indices[2, col]
        # The actual variables
        var1 <- merged_df[, ind1]
        var2 <- merged_df[, ind2]
        # The names of the variables
        var1_name <- colnames(merged_df)[ind1]
        var2_name <- colnames(merged_df)[ind2]
        # Types of the variables
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
        pval <- calculate_association_pval(
            var1,
            var2,
            var1_type,
            var2_type,
            cat_test
        )
        if (is.na(pval)) {
            stop(
                "Error returned when comparing ", var1_name, " with ",
                var2_name, ". Are you sure these are the correct types?"
            )
        }
        association_matrix[ind1, ind2] <- pval
        association_matrix[ind2, ind1] <- pval
    }
    if (!is.null(key_association)) {
        # The user has specified a key_association value.
        key_associations_df <- data.frame(
            name = names(association_matrix[key_association, ]),
            pval = association_matrix[key_association, ]
        )
        rownames(key_association) <- NULL
        key_associations_df <- dplyr::inner_join(
            key_associations_df,
            metadata,
            by = "name"
        )
        # Remove the 0 p-value for the association of the variable with self
        if (drop_self) {
            keep_rows <- key_associations_df$"name" != key_association
            key_associations_df <- key_associations_df[keep_rows, ]
        }
        return(key_associations_df)
    } else {
        # The user has not specified a key_association value, provide the full
        #  pairwise association matrix.
        return(association_matrix)
    }
}
