#' Generate a list of distance metrics
#'
#' This function can be used to specify custom distance metrics
#'
#' @param continuous_distances A named list of distance metric functions
#' @param discrete_distances A named list of distance metric functions
#' @param ordinal_distances A named list of distance metric functions
#' @param categorical_distances A named list of distance metric functions
#' @param mixed_distances A named list of distance metric functions
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
#'     continuous_distances = list(
#'          "my_distance_metric" = my_distance_metric
#'     )
#' )
#'
#' # Suppress the base metrics-------------------------------------------------
#' # This will contain only user-provided clustering algorithms
#'
#' distance_metrics_list <- generate_distance_metrics_list(
#'     continuous_distances = list(
#'          "my_distance_metric" = my_distance_metric
#'     ),
#'     discrete_distances = list(
#'          "my_distance_metric" = my_distance_metric
#'     ),
#'     ordinal_distances = list(
#'          "my_distance_metric" = my_distance_metric
#'     ),
#'     categorical_distances = list(
#'          "my_distance_metric" = my_distance_metric
#'     ),
#'     mixed_distances = list(
#'          "my_distance_metric" = my_distance_metric
#'     ),
#'     keep_defaults = FALSE
#' )
#'
#' @export
generate_distance_metrics_list <- function(continuous_distances = NULL,
                                           discrete_distances = NULL,
                                           ordinal_distances = NULL,
                                           categorical_distances = NULL,
                                           mixed_distances = NULL,
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
        continuous_distances,
        discrete_distances,
        ordinal_distances,
        categorical_distances,
        mixed_distances
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
        stop(
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
            stop(
                paste0(
                    "If suppressing base distance metrics, you must specify",
                    " at least one metric for each feature type (continuous,",
                    " discrete, ordinal, categorical, and mixed) even if you",
                    " are not intending on using that type."
                )
            )
        }
    }
    ###########################################################################
    # 2. Set up the default lists
    base_continuous_distances <- list(
        "euclidean_distance" = euclidean_distance
    )
    base_discrete_distances <- list(
        "euclidean_distance" = euclidean_distance
    )
    base_ordinal_distances <- list(
        "euclidean_distance" = euclidean_distance
    )
    base_categorical_distances <- list(
        "gower_distance" = gower_distance
    )
    base_mixed_distances <- list(
        "gower_distance" = gower_distance
    )
    ###########################################################################
    # 3. Add any user provided lists
    if (!is.null(continuous_distances)) {
        # the user provided continuous_distances
        if (keep_defaults) {
            # the user wants default metrics included
            continuous_distances <- c(
                base_continuous_distances,
                continuous_distances
            )
        } # no need for an else here, just leave their distances alone
    } else {
        # the user did not provide continuous_distances
        if (keep_defaults) {
            # the user wants default metrics included
            continuous_distances <- base_continuous_distances
        } else {
            # the user wants nothing
            continuous_distances <- list(NULL)
        }
    }
    if (!is.null(discrete_distances)) {
        # the user provided discrete_distances
        if (keep_defaults) {
            # the user wants default metrics included
            discrete_distances <- c(
                base_discrete_distances,
                discrete_distances
            )
        } # no need for an else here, just leave their distances alone
    } else {
        # the user did not provide discrete_distances
        if (keep_defaults) {
            # the user wants default metrics included
            discrete_distances <- base_discrete_distances
        } else {
            # the user wants nothing
            discrete_distances <- list(NULL)
        }
    }
    if (!is.null(ordinal_distances)) {
        # the user provided ordinal_distances
        if (keep_defaults) {
            # the user wants default metrics included
            ordinal_distances <- c(
                base_ordinal_distances,
                ordinal_distances
            )
        } # no need for an else here, just leave their distances alone
    } else {
        # the user did not provide ordinal_distances
        if (keep_defaults) {
            # the user wants default metrics included
            ordinal_distances <- base_ordinal_distances
        } else {
            # the user wants nothing
            ordinal_distances <- list(NULL)
        }
    }
    if (!is.null(categorical_distances)) {
        # the user provided categorical_distances
        if (keep_defaults) {
            # the user wants default metrics included
            categorical_distances <- c(
                base_categorical_distances,
                categorical_distances
            )
        } # no need for an else here, just leave their distances alone
    } else {
        # the user did not provide categorical_distances
        if (keep_defaults) {
            # the user wants default metrics included
            categorical_distances <- base_categorical_distances
        } else {
            # the user wants nothing
            categorical_distances <- list(NULL)
        }
    }
    if (!is.null(mixed_distances)) {
        # the user provided mixed_distances
        if (keep_defaults) {
            # the user wants default metrics included
            mixed_distances <- c(
                base_mixed_distances,
                mixed_distances
            )
        } # no need for an else here, just leave their distances alone
    } else {
        # the user did not provide mixed_distances
        if (keep_defaults) {
            # the user wants default metrics included
            mixed_distances <- base_mixed_distances
        } else {
            # the user wants nothing
            mixed_distances <- list(NULL)
        }
    }
    distance_metrics_list <- list(
        "continuous_distances" = continuous_distances,
        "discrete_distances" = discrete_distances,
        "ordinal_distances" = ordinal_distances,
        "categorical_distances" = categorical_distances,
        "mixed_distances" = mixed_distances
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
#' @param df Dataframe containing one subjectkey column in the first column and
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
