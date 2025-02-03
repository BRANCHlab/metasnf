#' Constructor for `solutions_df` class object
#'
#' @keywords internal
#' @param sol_dfl A solutions data frame-like object to be validated and
#'  converted into a solutions data frame.
#' @param smll A similarity matrix list-like object to be validated and used
#'  to construct a solutions data frame.
#' @param sc An `snf_config` object used to construct a solutions data frame.
#' @param dl An `data_list` object used to construct a solutions data frame.
#' @return A solutions data frame (`solutions_df` class object).
solutions_df <- function(sol_dfl, smll, sc, dl) {
    smll <- validate_sim_mats_list(smll)
    sml <- new_sim_mats_list(smll)
    attributes(sol_dfl)$"sim_mats_list" <- sml
    attributes(sol_dfl)$"snf_config" <- sc
    sol_dfl$"solution" <- as.numeric(sol_dfl$"solution")
    sol_dfl$"mc" <- as.character(sol_dfl$"mc")
    sol_dfl <- dplyr::mutate(
        sol_dfl,
        dplyr::across(
            dplyr::starts_with("uid_"), as.numeric
        )
    )
    sol_dfl <- validate_solutions_df(sol_dfl)
    sol_df <- new_solutions_df(sol_dfl)
    return(sol_df)
}

#' Validator for `solutions_df` class object
#'
#' @inheritParams solutions_df
#' @return If sol_dfl has a valid structure for a `solutions_df` class object, 
#'  returns the input unchanged. Otherwise, raises an error.
#' @export
validate_solutions_df <- function(sol_dfl) {
    class(sol_dfl)  <- setdiff(class(sol_dfl), "solutions_df")
    if (!identical(colnames(sol_dfl)[1:3], c("solution", "nclust", "mc"))) {
        metasnf_error(
            "First three columns of `solutions_df` must be \"solution\",",
            " \"nclust\", and \"mc\"."
        )
    }
    if (sum(grepl("^uid_", colnames(sol_dfl))) == 0) {
        metasnf_error(
            "`solutions_df` must have at least one observation (uid) column."
        )
    }
    return(sol_dfl)
}

#' Constructor for `solutions_df` class object
#'
#' @inheritParams solutions_df 
#' @return A `solutions_df` class object.
#' @export
new_solutions_df <- function(sol_dfl) {
    sol_df <- structure(sol_dfl, class = c("solutions_df", "data.frame"))    
    return(sol_df)
}

#' Get p-values from an extended solutions data frame
#'
#' This function can be used to neatly format the p-values associated with an
#' extended solutions data frame. It can also calculate the negative logs of those
#' p-values to make it easier to interpret large-scale differences.
#'
#' @param ext_sol_df The output of `extend_solutions`. A
#'  data frame that contains at least one p-value column ending in "_pval".
#' @param negative_log If TRUE, will replace p-values with negative log
#'  p-values.
#' @param keep_summaries If FALSE, will remove the mean, min, and max p-value.
#' @return A "data.frame" class object Of only the p-value related columns
#' of the provided ext_sol_df.
#' @export
get_pvals <- function(ext_sol_df,
                      negative_log = FALSE,
                      keep_summaries = TRUE) {
    # Select p-value columns and convert to numeric
    pval_df <- ext_sol_df |>
        gselect(c("^solution$", "_pval$")) |>
        data.frame() |>
        numcol_to_numeric()
    # Convert p-values to negative log p-values if requested
    if (negative_log) {
        # Negative log conversions
        neg_log_pval_df <- -log(pval_df)
        neg_log_pval_df$"solution" <- pval_df$"solution"
        return(neg_log_pval_df)
    }
    if (!keep_summaries) {
        pval_df <- drop_cols(pval_df, c("mean_pval", "min_pval", "max_pval"))
    }
    return(pval_df)
}

#' Summarize p-value columns of an extended solutions data frame
#'
#' @keywords internal
#' @param ext_sol_df Result of `extend_solutions`
#' @return The provided extended solutions data frame along with columns for
#'  the min, mean, and maximum across p-values for each row.
summarize_pvals <- function(ext_sol_df) {
    # Restrict to just p-value columns
    pval_cols <- gselect(ext_sol_df, "_pval$") |>
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
#' @keywords internal
#' @param sol_df_row row of sol_df object
#' @return min_pval minimum p-value
get_min_pval <- function(sol_df_row) {
    min_pval <- sol_df_row |>
        dplyr::mutate(
            dplyr::across(dplyr::ends_with("_pval"), ~ as.numeric(.))
        ) |>
        gselect("_pval$") |>
        min()
    return(min_pval)
}

#' Get mean p-value
#'
#' Given an solutions data frame row containing evaluated p-values, returns mean.
#'
#' @keywords internal
#' @param sol_df_row row of sol_df object
#' @return mean_pval mean p-value
get_mean_pval <- function(sol_df_row) {
    mean_pval <- sol_df_row |>
        dplyr::mutate(
            dplyr::across(dplyr::ends_with("_pval"), ~ as.numeric(.))
        ) |>
        gselect("_pval$") |>
        rowMeans()
    return(mean_pval)
}

#' Ordinal regression p-value
#'
#' Returns the overall p-value of an ordinal regression on a categorical
#' predictor and response vetors. If the ordinal response
#'
#' @keywords internal
#' @param predictor A categorical or numeric feature.
#' @param response A numeric feature.
#' @return pval A p-value (class "numeric").
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
#' @keywords internal
#' @param cat_var1 A categorical feature.
#' @param cat_var2 A categorical feature.
#' @return pval A p-value (class "numeric").
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
#' @keywords internal
#' @param cat_var1 A categorical feature.
#' @param cat_var2 A categorical feature.
#' @return pval A p-value (class "numeric").
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
#' @keywords internal
#' @param predictor A categorical or numeric feature.
#' @param response A numeric feature.
#' @return pval A p-value (class "numeric").
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
#' @keywords internal
#' @param var1 A single vector containing a feature.
#' @param var2 A single vector containing a feature.
#' @param type1 The type of var1 (continuous, discrete, ordinal, categorical).
#' @param type2 The type of var2 (continuous, discrete, ordinal, categorical).
#' @param cat_test String indicating which statistical test will be used to
#'  associate cluster with a categorical feature. Options are "chi_squared" for
#' the Chi-squared test and "fisher_exact" for Fisher's exact test.
#' @return pval A p-value from a statistical test based on the provided types.
#'  Currently, this will either be the F-test p-value from a linear model
#'  if at least one feature is non-categorical, or the chi-squared test
#'  p-value if both features are categorical.
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
#' @param verbose If TRUE, output progress to the console.
#' @param cat_test String indicating which statistical test will be used to
#'  associate cluster with a categorical feature. Options are "chi_squared" for
#'  the Chi-squared test and "fisher_exact" for Fisher's exact test.
#' @return A "matrix" class object containing pairwise association p-values
#'  between the features in the provided data list.
#' @export
#' @examples
#' data_list <- data_list(
#'     list(income, "household_income", "demographics", "ordinal"),
#'     list(pubertal, "pubertal_status", "demographics", "continuous"),
#'     list(anxiety, "anxiety", "behaviour", "ordinal"),
#'     list(depress, "depressed", "behaviour", "ordinal"),
#'     uid = "unique_id"
#' )
#' 
#' assoc_pval_matrix <- calc_assoc_pval_matrix(data_list)
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
        ## The positions of the two features in the merged data frame
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
        pval <- calc_assoc_pval(var1, var2, var1_type, var2_type, cat_test)
        association_matrix[ind1, ind2] <- pval
        association_matrix[ind2, ind1] <- pval
    }
    return(association_matrix)
}

#' Helper function for organizing solutions df-like column order
#'
#' Reorders columns of a solutions data frame to "solution", "nclust", "mc",
#' then all other column names.
#'
#' @keywords internal
#' @param x Object with columns "solution", "nclust", and "mc".
#' @return x with column names reordered.
sol_df_col_order <- function(x) {
    x <- x[, unique(c("solution", "nclust", "mc", colnames(x)))]
}
