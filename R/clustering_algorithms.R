#' Generate a list of custom clustering algorithms
#'
#' This function can be used to specify custom clustering algorithms to apply
#'  to the final affinity matrices produced by each run of the batch_snf
#'  function.
#'
#' @param ... An arbitrary number of named clustering functions (see examples
#'  below)
#' @param disable_base If TRUE, do not prepend the base clustering algorithms
#'  (spectral_eigen and spectral_rot, which apply spectral clustering and use
#'  the eigen-gap and rotation cost heuristics respectively for determining
#'  the number of clusters in the graph.
#'
#' @return clust_algs_list A well-formatted list of clustering algorithms that
#'  can be passed into the batch_snf and generate_settings_list functions.
#'
#' @examples
#' # Using just the base clustering algorithms --------------------------------
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
#'     # return(solution)
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
generate_clust_algs_list <- function(..., disable_base = FALSE) {
    user_algs_list <- list(...)
    # Ensure that user has provided a name for the algorithm
    if (min(nchar(names(user_algs_list))) == 0) {
        stop(
            paste0(
                "Please specify a name for every supplied algorithm. See",
                " ?generate_clust_algs_list for examples."
            )
        )
    }
    base_algs_list <- list(
        "spectral_eigen" = spectral_eigen,
        "spectral_rot" = spectral_rot
    )
    clust_algs_list <- c(base_algs_list, user_algs_list)
    return(clust_algs_list)
}

#' Summarize a clust_algs_list object
#'
#' @param clust_algs_list A clust_algs_list object
#'
#' @return summary_df Dataframe containing the name of each algorithm and the
#'  number of each algorithm (the value in the settings_matrix that will be
#'  used to access that algorithm)
#'
#' @export
summarize_clust_algs_list <- function(clust_algs_list) {
    summary_df <- data.frame(
        alg_number = seq_along(clust_algs_list),
        algorithm = names(clust_algs_list)
    )
    return(summary_df)
}

#' Clustering algorithm: Spectral clustering with eigen-gap heuristic
#'
#' Applies spectral clustering to affinity matrix. Number of clusters is based
#'  on the eigen-gap heuristic.
#'
#' @param affinity_matrix An affinity matrix
#'
#' @return solution A vector indicating which cluster each patient was assigned
#'  to
#'
#' @export
spectral_eigen <- function(affinity_matrix) {
    estimated_n <- SNFtool::estimateNumberOfClustersGivenGraph(affinity_matrix)
    number_of_clusters <- estimated_n$`Eigen-gap best`
    solution <- SNFtool::spectralClustering(affinity_matrix, number_of_clusters)
    return(solution)
}

#' Clustering algorithm: Spectral clustering with rotation cost heuristic
#'
#' Applies spectral clustering to affinity matrix. Number of clusters is based
#'  on the rotation cost heuristic.
#'
#' @param affinity_matrix An affinity matrix
#'
#' @return solution A vector indicating which cluster each patient was assigned
#'  to
#'
#' @export
spectral_rot <- function(affinity_matrix) {
    estimated_n <- SNFtool::estimateNumberOfClustersGivenGraph(affinity_matrix)
    number_of_clusters <- estimated_n$`Rotation cost best`
    solution <- SNFtool::spectralClustering(affinity_matrix, number_of_clusters)
    return(solution)
}

#' Clustering algorithm: Spectral clustering for a two cluster solution
#'
#' Applies spectral clustering to affinity matrix. Seeks two clusters.
#'
#' @param affinity_matrix An affinity matrix
#'
#' @return solution A vector indicating which cluster each patient was assigned
#'  to
#'
#' @export
spectral_two <- function(affinity_matrix) {
    number_of_clusters <- 2
    solution <- SNFtool::spectralClustering(affinity_matrix, number_of_clusters)
    return(solution)
}

#' Clustering algorithm: Spectral clustering for a three cluster solution
#'
#' Applies spectral clustering to affinity matrix. Seeks three clusters.
#'
#' @param affinity_matrix An affinity matrix
#'
#' @return solution A vector indicating which cluster each patient was assigned
#'  to
#'
#' @export
spectral_three <- function(affinity_matrix) {
    number_of_clusters <- 3
    solution <- SNFtool::spectralClustering(affinity_matrix, number_of_clusters)
    return(solution)
}

#' Clustering algorithm: Spectral clustering for a four cluster solution
#'
#' Applies spectral clustering to affinity matrix. Seeks four clusters.
#'
#' @param affinity_matrix An affinity matrix
#'
#' @return solution A vector indicating which cluster each patient was assigned
#'  to
#'
#' @export
spectral_four <- function(affinity_matrix) {
    number_of_clusters <- 4
    solution <- SNFtool::spectralClustering(affinity_matrix, number_of_clusters)
    return(solution)
}

#' Clustering algorithm: Spectral clustering for a five cluster solution
#'
#' Applies spectral clustering to affinity matrix. Seeks five clusters.
#'
#' @param affinity_matrix An affinity matrix
#'
#' @return solution A vector indicating which cluster each patient was assigned
#'  to
#'
#' @export
spectral_five <- function(affinity_matrix) {
    number_of_clusters <- 5
    solution <- SNFtool::spectralClustering(affinity_matrix, number_of_clusters)
    return(solution)
}

#' Clustering algorithm: Spectral clustering for a six cluster solution
#'
#' Applies spectral clustering to affinity matrix. Seeks six clusters.
#'
#' @param affinity_matrix An affinity matrix
#'
#' @return solution A vector indicating which cluster each patient was assigned
#'  to
#'
#' @export
spectral_six <- function(affinity_matrix) {
    number_of_clusters <- 6
    solution <- SNFtool::spectralClustering(affinity_matrix, number_of_clusters)
    return(solution)
}

#' Clustering algorithm: Spectral clustering for a seven cluster solution
#'
#' Applies spectral clustering to affinity matrix. Seeks seven clusters.
#'
#' @param affinity_matrix An affinity matrix
#'
#' @return solution A vector indicating which cluster each patient was assigned
#'  to
#'
#' @export
spectral_seven <- function(affinity_matrix) {
    number_of_clusters <- 7
    solution <- SNFtool::spectralClustering(affinity_matrix, number_of_clusters)
    return(solution)
}

#' Clustering algorithm: Spectral clustering for a eight cluster solution
#'
#' Applies spectral clustering to affinity matrix. Seeks eight clusters.
#'
#' @param affinity_matrix An affinity matrix
#'
#' @return solution A vector indicating which cluster each patient was assigned
#'  to
#'
#' @export
spectral_eight <- function(affinity_matrix) {
    number_of_clusters <- 8
    solution <- SNFtool::spectralClustering(affinity_matrix, number_of_clusters)
    return(solution)
}
