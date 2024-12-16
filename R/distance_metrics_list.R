#' Generate a list of distance metrics
#'
#' This function can be used to specify custom distance metrics
#'
#' @param cnt_dist_fns A named list of distance metric functions
#' @param dsc_dist_fns A named list of distance metric functions
#' @param ord_dist_fns A named list of distance metric functions
#' @param cat_dist_fns A named list of distance metric functions
#' @param mix_dist_fns A named list of distance metric functions
#' @param keep_defaults If TRUE (default), prepend the base distance metrics
#'  (euclidean and standard normalized euclidean)
#'
#' @return distance_metrics_list A well-formatted list of distance metrics
#'
#' @examples
#' # Using just the base distance metrics  ------------------------------------
#' distance_metrics_list <- generate_distance_metrics_list()
#'
#' # Adding your own metrics --------------------------------------------------
#' # This will contain the base and user-provided clustering algorithms
#' my_distance_metric <- function(df) {
#'     # your code that converts a dataframe to a distance metric here...
#'     # return(distance_metric)
#' }
#'
#' distance_metrics_list <- generate_distance_metrics_list(
#'     cnt_dist_fns = list(
#'          "my_distance_metric" = my_distance_metric
#'     )
#' )
#'
#' # Suppress the base metrics-------------------------------------------------
#' # This will contain only user-provided clustering algorithms
#'
#' distance_metrics_list <- generate_distance_metrics_list(
#'     cnt_dist_fns = list(
#'          "my_distance_metric" = my_distance_metric
#'     ),
#'     dsc_dist_fns = list(
#'          "my_distance_metric" = my_distance_metric
#'     ),
#'     ord_dist_fns = list(
#'          "my_distance_metric" = my_distance_metric
#'     ),
#'     cat_dist_fns = list(
#'          "my_distance_metric" = my_distance_metric
#'     ),
#'     mix_dist_fns = list(
#'          "my_distance_metric" = my_distance_metric
#'     ),
#'     keep_defaults = FALSE
#' )
#'
#' @export
generate_distance_metrics_list <- function(cnt_dist_fns = NULL,
                                           dsc_dist_fns = NULL,
                                           ord_dist_fns = NULL,
                                           cat_dist_fns = NULL,
                                           mix_dist_fns = NULL,
                                           keep_defaults = TRUE) {
    # The code below is repetitive across the different types of distance
    #  metrics. For each type of metric, the following logic is applied to fill
    #  each type of distance metric in the broader distance_metrics_list:
    #  1. User metrics + !disable_base = base metrics + user metrics
    #  2. User metrics + disable_base = only user metrics
    #  3. No user metrics + !disable_base = base metrics
    #  4. No user metrics + disable_base = NULL
    ###########################################################################
    # 1. Start with a check to ensure any list provided by the user has named
    #  elements.
    user_distances <- list(
        cnt_dist_fns,
        dsc_dist_fns,
        ord_dist_fns,
        cat_dist_fns,
        mix_dist_fns
    )
    # Remove the NULL default elements
    user_distances <- user_distances[lengths(user_distances) != 0]
    # Check that all the elements WITHIN the lists provided by the users have
    #  names.
    all_metrics_are_named <- user_distances |>
        lapply(
            function(x) {
                sum(nchar(names(x)) > 0) == length(x)
            }
        ) |>
        unlist() |>
        all()
    if (!all_metrics_are_named) {
        metasnf_error(
            paste0(
                "Please specify a name for every supplied metric."
            )
        )
    }
    ###########################################################################
    # 2. Ensure that if the user is not using the defaults, that at least one
    #  metric is provided for each feature type
    if (!keep_defaults) {
        if (length(user_distances) < 5) {
            metasnf_error(
                "If suppressing base distance metrics, you must specify",
                " at least one metric for each feature type (continuous,",
                " discrete, ordinal, categorical, and mixed) even if you",
                " are not intending on using that type."
            )
        }
    }
    ###########################################################################
    # 2. Set up the default lists
    base_cnt_dist_fns <- list(
        "euclidean_distance" = euclidean_distance
    )
    base_dsc_dist_fns <- list(
        "euclidean_distance" = euclidean_distance
    )
    base_ord_dist_fns <- list(
        "euclidean_distance" = euclidean_distance
    )
    base_cat_dist_fns <- list(
        "gower_distance" = gower_distance
    )
    base_mix_dist_fns <- list(
        "gower_distance" = gower_distance
    )
    ###########################################################################
    # 3. Add any user provided lists
    if (!is.null(cnt_dist_fns)) {
        # the user provided cnt_dist_fns
        if (keep_defaults) {
            # the user wants default metrics included
            cnt_dist_fns <- c(
                base_cnt_dist_fns,
                cnt_dist_fns
            )
        } # no need for an else here, just leave their distances alone
    } else {
        # the user did not provide cnt_dist_fns
        if (keep_defaults) {
            # the user wants default metrics included
            cnt_dist_fns <- base_cnt_dist_fns
        } else {
            # the user wants nothing
            cnt_dist_fns <- list(NULL)
        }
    }
    if (!is.null(dsc_dist_fns)) {
        # the user provided dsc_dist_fns
        if (keep_defaults) {
            # the user wants default metrics included
            dsc_dist_fns <- c(
                base_dsc_dist_fns,
                dsc_dist_fns
            )
        } # no need for an else here, just leave their distances alone
    } else {
        # the user did not provide dsc_dist_fns
        if (keep_defaults) {
            # the user wants default metrics included
            dsc_dist_fns <- base_dsc_dist_fns
        } else {
            # the user wants nothing
            dsc_dist_fns <- list(NULL)
        }
    }
    if (!is.null(ord_dist_fns)) {
        # the user provided ord_dist_fns
        if (keep_defaults) {
            # the user wants default metrics included
            ord_dist_fns <- c(
                base_ord_dist_fns,
                ord_dist_fns
            )
        } # no need for an else here, just leave their distances alone
    } else {
        # the user did not provide ord_dist_fns
        if (keep_defaults) {
            # the user wants default metrics included
            ord_dist_fns <- base_ord_dist_fns
        } else {
            # the user wants nothing
            ord_dist_fns <- list(NULL)
        }
    }
    if (!is.null(cat_dist_fns)) {
        # the user provided cat_dist_fns
        if (keep_defaults) {
            # the user wants default metrics included
            cat_dist_fns <- c(
                base_cat_dist_fns,
                cat_dist_fns
            )
        } # no need for an else here, just leave their distances alone
    } else {
        # the user did not provide cat_dist_fns
        if (keep_defaults) {
            # the user wants default metrics included
            cat_dist_fns <- base_cat_dist_fns
        } else {
            # the user wants nothing
            cat_dist_fns <- list(NULL)
        }
    }
    if (!is.null(mix_dist_fns)) {
        # the user provided mix_dist_fns
        if (keep_defaults) {
            # the user wants default metrics included
            mix_dist_fns <- c(
                base_mix_dist_fns,
                mix_dist_fns
            )
        } # no need for an else here, just leave their distances alone
    } else {
        # the user did not provide mix_dist_fns
        if (keep_defaults) {
            # the user wants default metrics included
            mix_dist_fns <- base_mix_dist_fns
        } else {
            # the user wants nothing
            mix_dist_fns <- list(NULL)
        }
    }
    distance_metrics_list <- list(
        "cnt_dist_fns" = cnt_dist_fns,
        "dsc_dist_fns" = dsc_dist_fns,
        "ord_dist_fns" = ord_dist_fns,
        "cat_dist_fns" = cat_dist_fns,
        "mix_dist_fns" = mix_dist_fns
    )
    return(distance_metrics_list)
}

#' Summarize metrics contained in a distance_metrics_list
#'
#' @param distance_metrics_list A distance_metrics_list.
#'
#' @return "data.frame"-class object summarizing items in a distance metrics
#' list.
#'
#' @export
summarize_dml <- function(distance_metrics_list) {
    dml_summary <- lapply(distance_metrics_list, names)
    return(dml_summary)
}

#' Distance metric: Euclidean distance
#'
#' @param df Dataframe containing at least 1 data column
#' @param weights_row Single-row dataframe where the column names contain the
#'  column names in df and the row contains the corresponding weights_row.
#'
#' @return distance_matrix A distance matrix.
#'
#' @export
euclidean_distance <- function(df, weights_row) {
    weights <- diag(weights_row, nrow = length(weights_row))
    weighted_df <- as.matrix(df) %*% weights
    distance_matrix <- weighted_df |>
        stats::dist(method = "euclidean") |>
        as.matrix()
    return(distance_matrix)
}

#' Distance metric: Gower distance
#'
#' @param df Dataframe containing at least 1 data column.
#' @param weights_row For compatibility - function does not accept weights.
#'
#' @return distance_matrix A distance matrix.
#'
#' @export
gower_distance <- function(df, weights_row) {
    df <- char_to_fac(df)
    distance_matrix <- df |>
        cluster::daisy(metric = "gower", warnBin = FALSE) |>
        as.matrix()
    return(distance_matrix)
}

#' Distance metric: Standard normalization then Euclidean
#'
#' @param df Dataframe containing at least 1 data column.
#' @param weights_row Single-row dataframe where the column names contain the
#'  column names in df and the row contains the corresponding weights.
#'
#' @return distance_matrix A distance matrix.
#'
#' @export
sn_euclidean_distance <- function(df, weights_row) {
    df <- SNFtool::standardNormalization(df)
    weights <- diag(weights_row, nrow = length(weights_row))
    weighted_df <- as.matrix(df) %*% weights
    distance_matrix <- weighted_df |>
        stats::dist(method = "euclidean") |>
        as.matrix()
    return(distance_matrix)
}

#' Squared (including weights) Euclidean distance
#'
#' @param df Dataframe containing at least 1 data column.
#' @param weights_row Single-row dataframe where the column names contain the
#'  column names in df and the row contains the corresponding weights.
#'
#' @return distance_matrix A distance matrix.
#'
#' @export
siw_euclidean_distance <- function(df, weights_row) {
    weights <- diag(weights_row, nrow = length(weights_row))
    weighted_df <- as.matrix(df) %*% weights
    distance_matrix <- weighted_df |>
        stats::dist(method = "euclidean") |>
        as.matrix()
    distance_matrix <- distance_matrix^2
    return(distance_matrix)
}

#' Squared (excluding weights) Euclidean distance
#'
#' @param df Dataframe containing at least 1 data column.
#' @param weights_row Single-row dataframe where the column names contain the
#'  column names in df and the row contains the corresponding weights.
#'
#' @return distance_matrix A distance matrix.
#'
#' @export
sew_euclidean_distance <- function(df, weights_row) {
    weights <- diag(weights_row, nrow = length(weights_row))
    weights <- sqrt(weights)
    weighted_df <- as.matrix(df) %*% weights
    distance_matrix <- weighted_df |>
        stats::dist(method = "euclidean") |>
        as.matrix()
    distance_matrix <- distance_matrix^2
    return(distance_matrix)
}

#' Distance metric: Hamming distance
#'
#' @param df Dataframe containing one uid column in the first column and
#'  at least 1 categorical data column. All feature data should be categorical.
#' @param weights_row Single-row dataframe where the column names contain the
#'  column names in df and the row contains the corresponding weights.
#'
#' @return distance_matrix A distance matrix.
#'
#' @export
hamming_distance <- function(df, weights_row) {
    weights <- t(weights_row)
    weights <- weights[, 1]
    distance_matrix <- sapply(
        seq_len(nrow(df)),
        function(i) {
            sapply(
                seq_len(nrow(df)),
                function(j) {
                    apply_condition <- df[i, ] != df[j, ]
                    apply_condition <- as.numeric(apply_condition)
                    return(apply_condition %*% weights)
                }
            )
        }
    )
    return(distance_matrix)
}
