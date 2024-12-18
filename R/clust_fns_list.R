#' Build a clusteing algorithms list
#'
#' This function can be used to specify custom clustering algorithms to apply
#' to the final similarity matrices produced by each run of the batch_snf
#' function.
#'
#' @param clust_fns A list of named clustering functions
#' @param use_default_clust_fns If TRUE, prepend the base clustering algorithms
#'  (spectral_eigen and spectral_rot, which apply spectral clustering and use
#'  the eigen-gap and rotation cost heuristics respectively for determining
#'  the number of clusters in the graph) to clust_fns.
#' @return A list of clustering algorithm functions that can
#'  be passed into the batch_snf and generate_settings_list functions.
#' @examples
#' # Using just the base clustering algorithms --------------------------------
#' # This will just contain spectral_eigen and spectral_rot
#' cfl <- clust_fns_list(use_default_clust_fns = TRUE)
#'
#' # Adding algorithms provided by the package --------------------------------
#' # This will contain the base clustering algorithms (spectral_eigen,
#' #  spectral_rot) as well as two pre-defined spectral clustering functions
#' #  that force the number of clusters to be two or five
#' cfl <- clust_fns_list(
#'      clust_fns = list(
#'         "two_cluster_spectral" = spectral_two,
#'         "five_cluster_spectral" = spectral_five
#'     )
#' )
#'
#' # Adding your own algorithms -----------------------------------------------
#' # This will contain the base and user-provided clustering algorithms
#' my_clustering_algorithm <- function(similarity_matrix) {
#'     # your code that converts similarity matrix to clusters here...
#'     # solution_data <- list(
#'     #     "solution" = solution,
#'     #     "nclust" = number_of_clusters
#'     # )
#'     # return(solution_data)
#' }
#'
#' # Suppress the base algorithms----------------------------------------------
#' # This will contain only user-provided clustering algorithms
#' cfl <- clust_fns_list(
#'     clust_fns = list(
#'         "two_cluster_spectral" = spectral_two,
#'         "five_cluster_spectral" = spectral_five
#'     )
#' )
#' @export
clust_fns_list <- function(clust_fns = NULL, use_default_clust_fns = FALSE) {
    cfl <- clust_fns
    if (use_default_clust_fns) {
        base_algs_list <- list(
            "spectral_eigen" = spectral_eigen,
            "spectral_rot" = spectral_rot
        )
        cfl <- c(base_algs_list, cfl)
    }
    return(cfl)
}

#' Check if clustering functions list-like object has named algorithms
#'
#' @keywords internal
#' @param cfll A clustering functions list-like `list` class object.
#' @return Doesn't return any value. Raises error if there are unnamed
#'  clustering functions in cfll.
check_cfll_named <- function(cfll) {
    # Ensure that user has provided a name for the algorithm
    if (min(nchar(names(cfll))) == 0) {
        metasnf_error("Please specify a name for every supplied function.")
    }
}

#' Summarize a clust_fns_list object
#'
#' @param cfl A `clust_fns_list` class object.
#' @return summary_df "data.frame" class object containing the name and index
#'  of each clustering algorithm in te provided `clust_fns_list`.
#' @export
summarize_clust_fns_list <- function(cfl) {
    summary_df <- data.frame(
        alg_number = seq_along(cfl),
        algorithm = names(cfl)
    )
    return(summary_df)
}

#' Clustering algorithm: Spectral clustering with eigen-gap heuristic
#'
#' Applies spectral clustering to similarity matrix. Number of clusters is based
#' on the eigen-gap heuristic.
#'
#' @param similarity_matrix A similarity matrix.
#'
#' @return solution_data A list storing cluster assignments ("solution") and
#' the number of clusters ("nclust").
#'
#' @export
spectral_eigen <- function(similarity_matrix) {
    estimated_n <- estimate_nclust_given_graph(
        W = similarity_matrix,
        NUMC = 2:10
    )
    nclust_estimate <- estimated_n$`Eigen-gap best`
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        nclust_estimate
    )
    nclust <- length(unique(solution))
    solution_data <- list("solution" = solution, "nclust" = nclust)
    return(solution_data)
}

#' Clustering algorithm: Spectral clustering with rotation cost heuristic
#'
#' Applies spectral clustering to similarity matrix. Number of clusters is based
#'  on the rotation cost heuristic.
#'
#' @param similarity_matrix A similarity matrix.
#'
#' @return solution_data A list storing cluster assignments ("solution") and
#' the number of clusters ("nclust").
#'
#' @export
spectral_rot <- function(similarity_matrix) {
    estimated_n <- estimate_nclust_given_graph(
        W = similarity_matrix,
        NUMC = 2:10
    )
    nclust_estimate <- estimated_n$`Rotation cost best`
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        nclust_estimate
    )
    nclust <- length(unique(solution))
    solution_data <- list("solution" = solution, "nclust" = nclust)
    return(solution_data)
}

#' Clustering algorithm: Spectral clustering with eigen-gap heuristic
#'
#' Applies spectral clustering to similarity matrix. Number of clusters is based
#' on the eigen-gap heuristic. Range of possible cluster solutions is fixed
#' between 2 and 5 inclusive.
#'
#' @param similarity_matrix A similarity matrix.
#'
#' @return solution_data A list storing cluster assignments ("solution") and
#' the number of clusters ("nclust").
#'
#' @export
spectral_eigen_classic <- function(similarity_matrix) {
    estimated_n <- estimate_nclust_given_graph(
        W = similarity_matrix,
        NUMC = 2:5
    )
    nclust_estimate <- estimated_n$`Eigen-gap best`
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        nclust_estimate
    )
    nclust <- length(unique(solution))
    solution_data <- list("solution" = solution, "nclust" = nclust)
    return(solution_data)
}

#' Clustering algorithm: Spectral clustering with rotation cost heuristic
#'
#' Applies spectral clustering to similarity matrix. Number of clusters is based
#' on the rotation cost heuristic.
#'
#' @param similarity_matrix A similarity matrix.
#'
#' @return solution_data A list storing cluster assignments ("solution") and
#' the number of clusters ("nclust").
#'
#' @export
spectral_rot_classic <- function(similarity_matrix) {
    estimated_n <- estimate_nclust_given_graph(
        W = similarity_matrix,
        NUMC = 2:5
    )
    nclust_estimate <- estimated_n$`Rotation cost best`
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        nclust_estimate
    )
    nclust <- length(unique(solution))
    solution_data <- list("solution" = solution, "nclust" = nclust)
    return(solution_data)
}

#' Clustering algorithm: Spectral clustering for a two cluster solution
#'
#' Applies spectral clustering to similarity matrix. Seeks two clusters.
#'
#' @param similarity_matrix A similarity matrix.
#'
#' @return solution_data A list storing cluster assignments ("solution") and
#' the number of clusters ("nclust").
#'
#' @export
spectral_two <- function(similarity_matrix) {
    number_of_clusters <- 2
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        number_of_clusters
    )
    nclust <- length(unique(solution))
    solution_data <- list("solution" = solution, "nclust" = nclust)
    if (number_of_clusters != nclust) {
        metasnf_warning(
            "Spectral clustering provided a solution of size ", nclust,
            " when the number requested was 2."
        )
    }
    return(solution_data)
}

#' Clustering algorithm: Spectral clustering for a three cluster solution
#'
#' Applies spectral clustering to similarity matrix. Seeks three clusters.
#'
#' @param similarity_matrix A similarity matrix.
#'
#' @return solution_data A list storing cluster assignments ("solution") and
#' the number of clusters ("nclust").
#'
#' @export
spectral_three <- function(similarity_matrix) {
    number_of_clusters <- 3
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        number_of_clusters
    )
    nclust <- length(unique(solution))
    solution_data <- list("solution" = solution, "nclust" = nclust)
    if (number_of_clusters != nclust) {
        metasnf_warning(
            "Spectral clustering provided a solution of size ", nclust,
            " when the number requested was 3."
        )
    }
    return(solution_data)
}

#' Clustering algorithm: Spectral clustering for a four cluster solution
#'
#' Applies spectral clustering to similarity matrix. Seeks four clusters.
#'
#' @param similarity_matrix A similarity matrix.
#'
#' @return solution_data A list storing cluster assignments ("solution") and
#' the number of clusters ("nclust").
#'
#' @export
spectral_four <- function(similarity_matrix) {
    number_of_clusters <- 4
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        number_of_clusters
    )
    nclust <- length(unique(solution))
    solution_data <- list("solution" = solution, "nclust" = nclust)
    if (number_of_clusters != nclust) {
        metasnf_warning(
            "Spectral clustering provided a solution of size ", nclust,
            " when the number requested was 4."
        )
    }
    return(solution_data)
}

#' Clustering algorithm: Spectral clustering for a five cluster solution
#'
#' Applies spectral clustering to similarity matrix. Seeks five clusters.
#'
#' @param similarity_matrix A similarity matrix.
#'
#' @return solution_data A list storing cluster assignments ("solution") and
#' the number of clusters ("nclust").
#'
#' @export
spectral_five <- function(similarity_matrix) {
    number_of_clusters <- 5
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        number_of_clusters
    )
    nclust <- length(unique(solution))
    solution_data <- list("solution" = solution, "nclust" = nclust)
    if (number_of_clusters != nclust) {
        metasnf_warning(
            "Spectral clustering provided a solution of size ", nclust,
            " when the number requested was 5."
        )
    }
    return(solution_data)
}

#' Clustering algorithm: Spectral clustering for a six cluster solution
#'
#' Applies spectral clustering to similarity matrix. Seeks six clusters.
#'
#' @param similarity_matrix A similarity matrix.
#'
#' @return solution_data A list storing cluster assignments ("solution") and
#' the number of clusters ("nclust").
#'
#' @export
spectral_six <- function(similarity_matrix) {
    number_of_clusters <- 6
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        number_of_clusters
    )
    nclust <- length(unique(solution))
    solution_data <- list("solution" = solution, "nclust" = nclust)
    if (number_of_clusters != nclust) {
        metasnf_warning(
            "Spectral clustering provided a solution of size ", nclust,
            " when the number requested was 6."
        )
    }
    return(solution_data)
}

#' Clustering algorithm: Spectral clustering for a seven cluster solution
#'
#' Applies spectral clustering to similarity matrix. Seeks seven clusters.
#'
#' @param similarity_matrix A similarity matrix.
#'
#' @return solution_data A list storing cluster assignments ("solution") and
#' the number of clusters ("nclust").
#'
#' @export
spectral_seven <- function(similarity_matrix) {
    number_of_clusters <- 7
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        number_of_clusters
    )
    nclust <- length(unique(solution))
    solution_data <- list("solution" = solution, "nclust" = nclust)
    if (number_of_clusters != nclust) {
        metasnf_warning(
            "Spectral clustering provided a solution of size ", nclust,
            " when the number requested was 7."
        )
    }
    return(solution_data)
}

#' Clustering algorithm: Spectral clustering for a eight cluster solution
#'
#' Applies spectral clustering to similarity matrix. Seeks eight clusters.
#'
#' @param similarity_matrix A similarity matrix.
#'
#' @return solution_data A list storing cluster assignments ("solution") and
#' the number of clusters ("nclust").
#'
#' @export
spectral_eight <- function(similarity_matrix) {
    number_of_clusters <- 8
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        number_of_clusters
    )
    nclust <- length(unique(solution))
    solution_data <- list("solution" = solution, "nclust" = nclust)
    if (number_of_clusters != nclust) {
        metasnf_warning(
            "Spectral clustering provided a solution of size ", nclust,
            " when the number requested was 8."
        )
    }
    return(solution_data)
}

#' Clustering algorithm: Spectral clustering for a nine cluster solution
#'
#' Applies spectral clustering to similarity matrix. Seeks nine clusters.
#'
#' @param similarity_matrix A similarity matrix.
#'
#' @return solution_data A list storing cluster assignments ("solution") and
#' the number of clusters ("nclust").
#'
#' @export
spectral_nine <- function(similarity_matrix) {
    number_of_clusters <- 9
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        number_of_clusters
    )
    nclust <- length(unique(solution))
    solution_data <- list("solution" = solution, "nclust" = nclust)
    if (number_of_clusters != nclust) {
        metasnf_warning(
            "Spectral clustering provided a solution of size ", nclust,
            " when the number requested was 9."
        )
    }
    return(solution_data)
}

#' Clustering algorithm: Spectral clustering for a ten cluster solution
#'
#' Applies spectral clustering to similarity matrix. Seeks ten clusters.
#'
#' @param similarity_matrix A similarity matrix.
#'
#' @return solution_data A list storing cluster assignments ("solution") and
#' the number of clusters ("nclust").
#'
#' @export
spectral_ten <- function(similarity_matrix) {
    number_of_clusters <- 10
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        number_of_clusters
    )
    nclust <- length(unique(solution))
    solution_data <- list("solution" = solution, "nclust" = nclust)
    if (number_of_clusters != nclust) {
        metasnf_warning(
            "Spectral clustering provided a solution of size ", nclust,
            " when the number requested was 10."
        )
    }
    return(solution_data)
}
