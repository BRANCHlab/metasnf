#' Calculate distance matrices
#'
#' @description
#' Given a dataframe of numerical variables, return a euclidean distance matrix
#'
#' @param df Raw dataframe with subject IDs in column 1
#' @param input_type Either "numeric" (resulting in euclidean distances),
#'  "categorical" (resulting in binary distances), or "mixed" (resulting in
#'  gower distances)
#' @param scale Whether or not the data should be standard normalized prior to
#'  distance calculations
#' @param distance_metrics_list Output of generate_distance_metrics_list().
#'
#' @return dist_matrix Matrix of inter-observation distances
#'
#' @export
get_dist_matrix <- function(df,
                            input_type,
                            scale = FALSE,
                            distance_metrics_list = NULL) {
    # Move subject keys into dataframe rownames
    df <- data.frame(df, row.names = 1)
    if (input_type == "numeric") {
        if (scale) {
            df <- SNFtool::standardNormalization(df)
        }
        dist_matrix <- as.matrix(stats::dist(df, method = "euclidean"))
    } else if (input_type %in% c("mixed", "categorical")) {
        df <- char_to_fac(df)
        dist_matrix <-
            as.matrix(cluster::daisy(df, metric = "gower", warnBin = FALSE))
    } else {
        rlang::abort(
            paste0("The value ", input_type, " is not a valid input type."),
            class = "invalid_input")
    }
    return(dist_matrix)
}

#' Generate a list of distance metrics
#'
#' This function can be used to specify custom distance metrics
#'
#' @param continuous_distances A named list of distance metric functions
#' @param discrete_distances A named list of distance metric functions
#' @param ordinal_distances A named list of distance metric functions
#' @param categorical_distances A named list of distance metric functions
#' @param mixed_distances A named list of distance metric functions
#' @param disable_base If TRUE, do not prepend the base distance metrics
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
#'     disable_base = TRUE
#' )
#'
#' @export
generate_distance_metrics_list <- function(continuous_distances = NULL,
                                           discrete_distances = NULL,
                                           ordinal_distances = NULL,
                                           categorical_distances = NULL,
                                           mixed_distances = NULL,
                                           disable_base = FALSE) {
    # Ensure that user has provided a name for every distance metric
    if (!is.null(continuous_distances)) {
        if (min(nchar(names(continuous_distances))) == 0) {
            stop(
                paste0(
                    "Please specify a name for every supplied metric."
                )
            )
        }
    } else {
        continuous_distances <- list(
            "euclidean_distance" = euclidean_distance
        )
    }
    if (!is.null(discrete_distances)) {
        if (min(nchar(names(discrete_distances))) == 0) {
            stop(
                paste0(
                    "Please specify a name for every supplied metric."
                )
            )
        }
    } else {
        discrete_distances <- list(
            "euclidean_distance" = euclidean_distance
        )
    }
    if (!is.null(ordinal_distances)) {
        if (min(nchar(names(ordinal_distances))) == 0) {
            stop(
                paste0(
                    "Please specify a name for every supplied metric."
                )
            )
        }
    } else {
        ordinal_distances <- list(
            "euclidean_distance" = euclidean_distance
        )
    }
    if (!is.null(categorical_distances)) {
        if (min(nchar(names(categorical_distances))) == 0) {
            stop(
                paste0(
                    "Please specify a name for every supplied metric."
                )
            )
        }
    } else {
        categorical_distances <- list(
            "gower_distance" = gower_distance
        )
    }
    if (!is.null(mixed_distances)) {
        if (min(nchar(names(mixed_distances))) == 0) {
            stop(
                paste0(
                    "Please specify a name for every supplied metric."
                )
            )
        }
    } else {
        mixed_distances <- list(
            "gower_distance" = gower_distance
        )
    }
    distance_metrics_list <- list(
        "continuous_distances" = continuous_distances,
        "discrete_distances" = discrete_distances,
        "ordinal_distances" = ordinal_distances,
        "categorical_distances" = categorical_distances,
        "mixed_distances" = mixed_distances
    )
    return(distance_metrics_list)
    #distance_metrics_list M
    #base_distances_list <- list(
    #    "euclidean_distance" = euclidean_distance
    #)
    #continuous_distances_list <- c(base_distances_list, user_distances_list)
    #return(continuous_distances_list)
}

#' Summarize metrics contained in a distance_metrics_list
#'
#' @param distance_metrics_list A distance_metrics_list.
#'
#' @export
summarize_distance_metrics_list <- function(distance_metrics_list) {
    cat("\nContinuous distances:\n")
    distance_metrics_list$"continuous_distances" |>
        names() |>
        cat()
    cat("\n\nDiscrete distances:\n")
    distance_metrics_list$"discrete_distances" |>
        names() |>
        cat()
    cat("\n\nOrdinal distances:\n")
    distance_metrics_list$"ordinal_distances" |>
        names() |>
        cat()
    cat("\n\nCategorical distances:\n")
    distance_metrics_list$"categorical_distances" |>
        names() |>
        cat()
    cat("\n\nMixed distances:\n")
    distance_metrics_list$"mixed_distances" |>
        names() |>
        cat()
    cat("\n")
}

#' Distance metric: Euclidean distance
#'
#' @param df Dataframe containing one subjectkey column and at least 1 data
#'  column
#'
#' @return distance_matrix A distance matrix.
#'
#' @export
euclidean_distance <- function(df) {
    # Remove the first column, which is just the subjectkey
    df <- df[, -1]
    # Apply euclidean distance
    distance_matrix <- df |>
        stats::dist(method = "euclidean") |>
        as.matrix()
    return(distance_matrix)
}

#' Distance metric: Gower distance
#'
#' @param df Dataframe containing one subjectkey column and at least 1 data
#'  column
#'
#' @return distance_matrix A distance matrix.
#'
#' @export
gower_distance <- function(df) {
    # Remove the first column, which is just the subjectkey
    df <- df[, -1]
    # Convert all character columns into factors
    df <- char_to_fac(df)
    distance_matrix <- df |>
        cluster::daisy(metric = "gower", warnBin = FALSE) |>
        as.matrix()
}
