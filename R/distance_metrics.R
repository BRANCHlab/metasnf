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
#'
#' @return dist_matrix Matrix of inter-observation distances
#'
#' @export
get_dist_matrix <- function(df,
                            input_type,
                            scale = FALSE,
                            distance_list = NULL) {
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


#' Generate a list of continuous distance metrics
#'
#' This function can be used to specify custom continuous distance metrics to
#'  use during each run of the batch_snf
#'
#' @param ... An arbitrary number of named distance metrics functions (see examples
#'  below)
#' @param disable_base If TRUE, do not prepend the base distance metrics
#'  (euclidean and standard normalized euclidean)
#'
#' @return continuous_distances_list A well-formatted list of distance metrics
#'
#' @examples
#' # Using just the base distance metrics  ------------------------------------
#' # This will just contain spectral_eigen and spectral_rot
#' clust_algs_list <- generate_clust_algs_list()
#'
#' # Adding algorithms provided by the package --------------------------------
#' # This will contain the base clustering algorithms (spectral_eigen,
#' #  spectral_rot) as well as two pre-defined spectral clustering functions
#' #  that force the number of clusters to be two or five
#' clust_algs_list <- generate_clust_algs_list(
#'     "two_cluster_spectral" = spectral_two,
#'     "five_cluster_spectral" = spectral_five
#' )
#'
#' # Adding your own algorithms -----------------------------------------------
#' # This will contain the base and user-provided clustering algorithms
#' my_clustering_algorithm <- function(affinity_matrix) {
#'     # your code that converts affinity matrix to clusters here...
#'     # solution_data <- list("solution" = solution, "nclust" = number_of_clusters)
#'     # return(solution_data)
#' }
#'
#' # Suppress the base algorithms----------------------------------------------
#' # This will contain only user-provided clustering algorithms
#'
#' clust_algs_list <- generate_clust_algs_list(
#'     "two_cluster_spectral" = spectral_two,
#'     "five_cluster_spectral" = spectral_five,
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
    }
    #base_distances_list <- list(
    #    "euclidean_distance" = euclidean_distance
    #)
    #continuous_distances_list <- c(base_distances_list, user_distances_list)
    #return(continuous_distances_list)
}

euclidean_distance <- function(df) {
    # Remove the first column, which is just the subjectkey
    df <- df[, -1]
    # Apply euclidean distance
    distance_matrix <- df |>
        stats::dist(method = "euclidean") |>
        as.matrix()
    return(distance_matrix)
}

gower_distance <- function(df) {
    # Remove the first column, which is just the subjectkey
    df <- df[, -1]
    # Convert all character columns into factors
    df <- char_to_fac(df)
    distance_matrix <- df |>
        cluster::daisy(metric = "gower", warnBin = FALSE) |>
        as.matrix()
}
