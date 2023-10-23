#' Build solutions matrix
#'
#' @description
#' The matrix that will contain all the columns to be populated by SNF results
#'
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#' @param settings_matrix matrix indicating parameters to iterate SNF through
#'
#' @return solutions_matrix The solutions matrix
#'
#' @export
generate_solutions_matrix <- function(data_list, settings_matrix) {
    subjects <- list()
    for (i in seq_along(data_list)) {
        subjects <- append(subjects, (data_list[[i]][[1]]$"subjectkey"))
    }
    subjects <- unique(unlist(subjects))
    solutions_matrix <- add_columns(settings_matrix, subjects, 0)
    other_cols <- c(
        "eigen_best",
        "rot_best",
        "cbcl_nausea_p",
        "cbcl_vomiting_p",
        "cbcl_dizzy_p",
        "cbcl_overtired_p",
        "cbcl_sleeping_more_p",
        "cbcl_sleeping_less_p",
        "cbcl_depress_p",
        "cbcl_anxiety_p",
        "cbcl_attention_p",
        "cbcl_aggressive_p",
        "min_p_val",
        "mean_p_val")
    solutions_matrix <- add_columns(solutions_matrix, other_cols, 0)
    return(solutions_matrix)
}

#' Extend an solutions matrix to include outcome evaluations
#'
#' @param solutions_matrix an solutions_matrix
#' @param target_list an target_list
#'
#' @return extended_solutions_matrix an extended solutions matrix that contains
#'  p-value columns for each outcome in the provided target_list
#'
#' @export
extend_solutions <- function(solutions_matrix, target_list) {
    # Single vector of all feature names
    ol_features <- lapply(
        target_list,
        function(x) {
            # All the features from each target list dataframe
            colnames(x[[1]])[-1]
        }
    ) |> unlist()
    # Single vector of all feature types
    ol_feature_types <- lapply(
        target_list,
        function(x) {
            n_features <- ncol(x$"data") - 1
            outcome_type <- rep(x$"type", n_features)
        }
    ) |> unlist()
    # Add columns tracking p-values of all features
    # Specifying the dataframe structure avoids tibble-related errors
    solutions_matrix <- data.frame(solutions_matrix)
    solutions_matrix <- add_columns(
        solutions_matrix,
        paste0(ol_features, "_p"),
        fill = NA
    )
    # Single DF to contain all outcome features
    merged_df <- lapply(
        target_list,
        function(x) {
            x[[1]]
        }) |> merge_df_list()
    # Iterate across rows of the solutions matrix
    for (i in seq_len(nrow(solutions_matrix))) {
        clustered_subs <- get_clustered_subs(solutions_matrix[i, ])
        # This really shouldn't be different from clustered_subs
        assigned_subs <- clustered_subs |>
            dplyr::filter(clustered_subs$"cluster" != 0)
        # Iterate across each outcome measure included
        # Assign p-values
        for (j in 1:length(ol_features)) {
            #current_outcome_component <- target_list[[j]]
            current_outcome_component <- merged_df[, c(1, j + 1)]
            current_outcome_type <- ol_feature_types[j]
            current_outcome_name <- colnames(current_outcome_component)[2]
            p_value <- tryCatch(
                expr = {
                    p_value <- get_p(
                        assigned_subs,
                        current_outcome_component,
                        ol_feature_types[j],
                        ol_features[j]
                    )
                    p_value
                },
                warning = function(w, row = i) {
                    if(grep("Chi-squared", w$"message")) {
                        print(
                            paste0(
                                "In row ", row, ", the Chi-squared test",
                                " was applied on a table that had at least one",
                                " cell containing fewer than 5 elements.",
                                " Please note that when the expected number of",
                                " elements per cell is less than 5, an",
                                " assumption in the test is violated."
                            )
                        )
                        suppressWarnings(
                            p_value <- get_p(
                                assigned_subs,
                                current_outcome_component,
                                ol_feature_types[j],
                                ol_features[j]
                            )
                        )
                    } else {
                        p_value <- get_p(
                            assigned_subs,
                            current_outcome_component,
                            ol_feature_types[j],
                            ol_features[j]
                        )
                    }
                    p_value
                }
            )
            target_col <- grep(current_outcome_name, colnames(solutions_matrix))
            solutions_matrix[i, target_col] <- p_value
        }
        min_p <- get_min_p(solutions_matrix[i, ])
        mean_p <- get_mean_p(solutions_matrix[i, ])
        solutions_matrix[i, "min_p_val"] <- min_p
        solutions_matrix[i, "mean_p_val"] <- mean_p
    }
    return(solutions_matrix)
}

#' Select p-values from solutions matrix
#'
#' @param solutions_matrix The output of batch_snf
#'
#' @return p_val_matrix P-values ready for heatmap plotting
#'
#' @export
p_val_select <- function(solutions_matrix) {
    p_val_matrix <- solutions_matrix |>
        dplyr::select(
            "row_id",
            dplyr::ends_with("_p"),
            -c("min_p_val", "mean_p_val")) |>
        data.frame() |>
        numcol_to_numeric()
    return(p_val_matrix)
}

#' Get minimum p-value
#'
#' @description
#' Given an solutions matrix row containing evaluated p-values, returns minimum
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
#' @description
#' Given an solutions matrix row containing evaluated p-values, returns mean
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
#' @description
#' Depending on outcome measure, perform ordinal regression or linear regression
#'  and return p-value as a benchmark measure of how well-separated clusters
#'  are by the outcome measure
#'
#' @param assigned_subs dataframe of subjects who were assigned to a cluster
#'  and the cluster they were assigned to
#' @param outcome_df dataframe containing subjectkey and outcome feautre column
#' @param outcome_type string indicating the outcome type (numeric or ordinal)
#' @param outcome_name string indicating the name of the feature
#'
#' @return p_val the smallest p-value of interest
#'
#' @export
get_p <- function(assigned_subs, outcome_df, outcome_type, outcome_name) {
    if (outcome_type == "ordinal") {
        p_val <- ord_reg_p(assigned_subs, outcome_df, outcome_name)
    } else if (outcome_type == "numeric") {
        p_val <- lin_reg_p(assigned_subs, outcome_df, outcome_name)
    } else if (outcome_type == "categorical") {
        p_val <- chi_sq_p(assigned_subs, outcome_df, outcome_name)
    } else {
        stop(paste0(
            "Unsupported outcome type: ",
            outcome_type,
            ". Accepted types for now are numeric and ordinal."
        ))
    }
    return(p_val)
}

#' Ordinal regression p-value
#'
#' @description
#' Returns the p-value following an ordinal regression in which cluster
#'  is the IV and a provided ordinal variable is the DV
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

#' Linear regression p-value
#'
#' @description
#' Returns the p-value following an linear regression in which cluster
#'  is the IV and a provided ordinal variable is the DV
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
#' @description
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

#' Get clustered subjects
#'
#' @description
#' Pull a dataframe of clustered subjects from an solutions matrix structure
#'
#' @param solutions_matrix_row Output matrix row containing subtype membership
#'
#' @return clustered_subs Dataframe
#'
#' @export
get_clustered_subs <- function(solutions_matrix_row) {
    solutions_matrix_row <- data.frame(solutions_matrix_row)
    clustered_subs <-
        data.frame(
            t(
                solutions_matrix_row[
                    1,
                    which(startsWith(colnames(solutions_matrix_row), "subject_"))
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