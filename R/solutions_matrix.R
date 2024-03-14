#' Extend an solutions matrix to include outcome evaluations
#'
#' @param solutions_matrix A solutions_matrix.
#'
#' @param target_list A target_list.
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
            paste0(features, "_p"),
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
            clustered_subs <- get_clustered_subs(esm[i, ])
            for (j in seq_along(features)) {
                current_outcome_component <- merged_df[, c(1, j + 1)]
                current_outcome_name <- colnames(current_outcome_component)[2]
                suppressWarnings(
                    p_value <- get_cluster_pval(
                        clustered_subs,
                        current_outcome_component,
                        feature_types[j],
                        features[j],
                        cat_test = cat_test
                    )
                )
                target_col <- which(
                    paste0(current_outcome_name, "_p") == colnames(esm)
                )
                esm[i, target_col] <- p_value
            }
        }
    ###########################################################################
    # Parallel extension
    ###########################################################################
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
        # Iterate across rows of the solutions matrix
        future::plan(future::multisession, workers = processes)
        esm_rows <- future.apply::future_lapply(
            seq_len(nrow(esm)),
            function(i) {
                clustered_subs <- get_clustered_subs(esm[i, ])
                for (j in seq_along(features)) {
                    current_outcome_component <- merged_df[, c(1, j + 1)]
                    current_outcome_name <-
                        colnames(current_outcome_component)[2]
                    suppressWarnings(
                        p_value <- get_cluster_pval(
                            clustered_subs,
                            current_outcome_component,
                            feature_types[j],
                            features[j],
                            cat_test = cat_test
                        )
                    )
                    target_col <- which(
                        paste0(current_outcome_name, "_p") == colnames(esm)
                    )
                    esm[i, target_col] <- p_value
                }
                return(esm[i, ])
            }
        )
        future::plan(future::sequential)
        esm <- do.call("rbind", esm_rows)
    }
    ###########################################################################
    # If min_pval is assigned, replace any p-value less than this with min_pval
    ###########################################################################
    if (!is.null(min_pval)) {
        esm <- esm |>
            numcol_to_numeric() |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::ends_with("_p"),
                    ~ ifelse(. < min_pval, min_pval, .)
                )
            )
    }
    if (calculate_summaries) {
        esm <- pval_summaries(esm)
    }
    return(esm)
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

#' Select p-values from an extended solutions matrix
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
        pval_df <- dplyr::select(pval_df, -c("min_p_val", "mean_p_val"))
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

#' Add minimum and mean p-values to an extended solutions matrix
#'
#' @param solutions_matrix A solutions_matrix object that already has some
#' p-value columns included.
#'
#' @export
pval_summaries <- function(solutions_matrix) {
    pval_cols <- solutions_matrix |>
        dplyr::select(dplyr::ends_with("_p"))
    pval_cols <- numcol_to_numeric(pval_cols)
    mean_pvals <- apply(pval_cols, 1, FUN = mean)
    min_pvals <- apply(pval_cols, 1, FUN = min)
    solutions_matrix$"min_p_val" <- min_pvals
    solutions_matrix$"mean_p_val" <- mean_pvals
    return(solutions_matrix)
}

#' Get minimum p-value
#'
#' Given an solutions matrix row containing evaluated p-values, returns min.
#'
#' @param solutions_matrix_row row of solutions_matrix object
#'
#' @return min_p minimum p-value
#'
#' @export
get_min_p <- function(solutions_matrix_row) {
    min_p <- solutions_matrix_row |>
        dplyr::mutate(dplyr::across(dplyr::ends_with("_p"), ~ as.numeric(.))) |>
        dplyr::select(dplyr::ends_with("_p")) |>
        min()
    return(min_p)
}

#' Get mean p-value
#'
#' Given an solutions matrix row containing evaluated p-values, returns mean.
#'
#' @param solutions_matrix_row row of solutions_matrix object
#'
#' @return mean_p mean p-value
#'
#' @export
get_mean_p <- function(solutions_matrix_row) {
    mean_p <- solutions_matrix_row |>
        dplyr::mutate(dplyr::across(dplyr::ends_with("_p"), ~ as.numeric(.))) |>
        dplyr::select(dplyr::ends_with("_p")) |>
        rowMeans()
    return(mean_p)
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
#' @return p_val the smallest p-value of interest
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

#' Get p-value (deprecated)
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
#' @return p_val the smallest p-value of interest
#'
#' @export
get_p <- function(assigned_subs,
                  outcome_df,
                  outcome_type,
                  outcome_name,
                  cat_test = "chi_squared") {
    if (outcome_type == "ordinal") {
        p_val <- ord_reg_p(assigned_subs, outcome_df, outcome_name)
    } else if (outcome_type == "numeric") {
        p_val <- lin_reg_p(assigned_subs, outcome_df, outcome_name)
    } else if (outcome_type == "categorical") {
        if (cat_test == "chi_squared") {
            p_val <- chi_sq_p(assigned_subs, outcome_df, outcome_name)
        } else if (cat_test == "fisher_exact") {
            print(NULL)
        }
    } else {
        stop(paste0(
            "Unsupported outcome type: ",
            outcome_type,
            ". Accepted types for now are numeric and ordinal."
        ))
    }
    return(p_val)
}

#' Ordinal regression p-value (deprecated)
#'
#' Returns the p-value following an ordinal regression in which cluster
#'  is the IV and a provided ordinal variable is the DV.
#'
#' @param clust_membership Dataframe of cluster membership (get_clustered_subs)
#' @param outcome_df Dataframe containing DV
#' @param outcome_var DV as a string
#'
#' @return p_val The overall p-value distinguishing clusters by the DV
#'
#' @export
ord_reg_p <- function(clust_membership, outcome_df, outcome_var) {
    merged_df <-
        dplyr::inner_join(clust_membership, outcome_df, by = "subjectkey")
    num_classes <- length(unique(merged_df[, outcome_var]))
    # If there are only 2 tiers to the ordinal scale, just use linear model
    if (num_classes == 2) {
        return(lin_reg_p(clust_membership, outcome_df, outcome_var))
    }
    merged_df$"cluster" <- as.factor(merged_df$"cluster")
    merged_df[, outcome_var] <- as.ordered(merged_df[, outcome_var])
    null_model <- MASS::polr(merged_df[, outcome_var] ~ 1)
    full_model <- MASS::polr(merged_df[, outcome_var] ~ merged_df[, "cluster"])
    p_value <- stats::anova(null_model, full_model)$"Pr(Chi)"[2]
    return(p_value)
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

#' Linear regression p-value
#'
#' Returns the p-value following an linear regression in which cluster
#'  is the IV and a provided ordinal variable is the DV.
#'
#' @param clust_membership Dataframe of cluster membership (get_clustered_subs)
#' @param outcome_df Dataframe containing DV
#' @param outcome_var DV as a string
#'
#' @return p_val The overall p-value distinguishing clusters by the DV
#'
#' @export
lin_reg_p <- function(clust_membership, outcome_df, outcome_var) {
    merged_df <-
        dplyr::inner_join(clust_membership, outcome_df, by = "subjectkey")
    merged_df$"cluster" <- as.factor(merged_df$"cluster")
    model <- stats::lm(merged_df[, outcome_var] ~ merged_df[, "cluster"])
    fstat <- summary(model)$"fstatistic"
    p <- stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
    attributes(p) <- NULL
    return(p)
}

#' Chi-squared test p-value
#'
#' Returns the p-value following a chi-squared test (without Yates' continuity
#'  correction) on the distribution of a categorical variable by cluster.
#'
#' @param clust_membership Dataframe of cluster membership (get_clustered_subs)
#' @param outcome_df Dataframe containing outcome feature
#' @param outcome_var Outcome feature as a string
#'
#' @return p_val The chi-squared test p-value
#'
#' @export
chi_sq_p <- function(clust_membership, outcome_df, outcome_var) {
    # This dataframe merges clust_membership, which has the cluster of each
    #  column, with outcome_df, which has the data of each subject on the
    #  outcome feature being evaluated.
    merged_df <-
        dplyr::inner_join(clust_membership, outcome_df, by = "subjectkey")
    merged_df$"cluster" <- as.factor(merged_df$"cluster")
    model <- stats::chisq.test(
        merged_df[, "cluster"],
        merged_df[, outcome_var],
        correct = FALSE
    )
    p <- model$"p.value"
    return(p)
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
#' @param data_list data_list containing variables for pairwise associations.
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

#' Get clustered subjects
#'
#' Pull a dataframe of clustered subjects from an solutions matrix structure.
#'
#' @param solutions_matrix_row Output matrix row containing subtype membership
#'
#' @return clustered_subs Dataframe
#'
#' @export
get_clustered_subs <- function(solutions_matrix_row) {
    solutions_matrix_row <- data.frame(solutions_matrix_row)
    ###########################################################################
    # To-do: Re-write this for clarity
    ###########################################################################
    clustered_subs <-
        data.frame(
            t(
                solutions_matrix_row[
                    1,
                    which(
                        startsWith(
                            colnames(
                                solutions_matrix_row
                            ),
                            "subject_"
                        )
                    )
                ]
            )
        )
    clustered_subs$"subjectkey" <- rownames(clustered_subs)
    rownames(clustered_subs) <- NULL
    clustered_subs <- clustered_subs |>
        dplyr::select("subjectkey", dplyr::starts_with("X")) |>
        dplyr::rename("cluster" = dplyr::starts_with("X"))
    return(clustered_subs)
}
