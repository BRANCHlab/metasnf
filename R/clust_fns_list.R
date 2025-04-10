#' Build a clustering algorithms list
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
#' @export
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
clust_fns_list <- function(clust_fns = NULL, use_default_clust_fns = FALSE) {
    cfll <- clust_fns
    if (is.null(cfll) & !use_default_clust_fns) {
        metasnf_alert(
            "No clustering functions specified. Using defaults."
        )
        use_default_clust_fns <- TRUE
    }
    if (use_default_clust_fns) {
        base_algs_list <- list(
            "spectral_eigen" = spectral_eigen,
            "spectral_rot" = spectral_rot
        )
        cfll <- c(base_algs_list, cfll)
    }
    cfll <- validate_clust_fns_list(cfll)
    cfl <- new_clust_fns_list(cfll)
    return(cfl)
}

#' Constructor for `clust_fns_list` class object
#' 
#' @keywords internal
#' @param cfll A clust_fns_list-like `list` class object.
#' @return A `clust_fns_list` class object.
new_clust_fns_list <- function(cfll) {
    cfl <- structure(cfll, class = c("clust_fns_list", "list"))
    return(cfl)
}

#' Validator for `clust_fns_list` class object
#' 
#' @keywords internal
#' @inheritParams new_clust_fns_list
#' @return If cfll has a valid structure for a `clust_fns_list` class object, 
#'  returns the input unchanged. Otherwise, raises an error.
validate_clust_fns_list <- function(cfll) {
    class(cfll) <- setdiff(class(cfll), "clust_fns_list")
    check_cfll_named(cfll)
    check_cfll_unique_names(cfll)
    check_cfll_fns(cfll)
    check_cfll_fn_args(cfll)
    return(cfll)
}

#' Check if clustering functions list-like object has named algorithms
#'
#' @keywords internal
#' @inheritParams new_clust_fns_list
#' @return Doesn't return any value. Raises error if there are unnamed
#'  clustering functions in cfll.
check_cfll_named <- function(cfll) {
    if (min(nchar(names(cfll))) == 0) {
        metasnf_error("Please specify a name for every supplied function.")
    }
}

#' Check if names in a clustering functions list-like object are unique
#'
#' @keywords internal
#' @inheritParams new_clust_fns_list
#' @return Doesn't return any value. Raises error if the names in cfll aren't
#'  unique.
check_cfll_unique_names <- function(cfll) {
    n_names <- length(names(cfll))
    n_unique_names <- length(unique(names(cfll)))
    if (n_names != n_unique_names) {
        metasnf_error(
            "Clustering functions list cannot have duplicate function names."
        )
    }
}

#' Check if items of a clustering functions list-like object are functions
#'
#' @keywords internal
#' @inheritParams new_clust_fns_list
#' @return Doesn't return any value. Raises error if the items of cfll are
#'  not functions.
check_cfll_fns <- function(cfll) {
    items_are_fns <- lapply(
        cfll,
        function(x) {
            inherits(x, "function")
        }
    ) |>
        unlist() |>
        all()
    if (!items_are_fns) {
        metasnf_error("Clustering functions list can only store functions.")
    }
}

#' Check if functions in a distance metrics list-like have valid arguments
#'
#' @keywords internal
#' @inheritParams validate_dist_fns_list
#' @return Doesn't return any value. Raises error if the functions in dfll
#'  don't have valid arguments.
check_cfll_fn_args <- function(cfll) {
    valid_args <- lapply(
        cfll,
        function(x) {
            methods::formalArgs(x) == "similarity_matrix"
        }
    ) |>
        unlist() |>
        all()
    if (!valid_args) {
        metasnf_error(
            "Clustering functions list functions must only accept argument `s",
            "imilarity_matrix`."
        )
    }
}

#' Built-in clustering algorithms
#'
#' These functions can be used when building a `metasnf` clustering functions
#' list. Each function converts a similarity matrix (matrix class object) to a
#' cluster solution (numeric vector). Note that these functions (or custom
#' clustering functions) cannot accept number of clusters as a parameter; this
#' value must be built into the function itself if necessary.
#'
#' - spectral_eigen: Spectral clustering where the number of clusters is based
#'   on the eigen-gap heuristic
#' - spectral_rot: Spectral clustering where the number of clusters is based
#'   on the rotation-cost heuristic
#' - spectral_(C): Spectral clustering for a C-cluster solution.
#'
#' @param similarity_matrix A similarity matrix.
#' @return solution_data A vector of cluster assignments
#' @name clust_fns
NULL

#' @rdname clust_fns
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
    return(solution)
}

#' @rdname clust_fns
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
    return(solution)
}

#' @rdname clust_fns
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
    return(solution)
}

#' @rdname clust_fns
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
    return(solution)
}

#' @rdname clust_fns
#' @export
spectral_two <- function(similarity_matrix) {
    number_of_clusters <- 2
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        number_of_clusters
    )
    nclust <- length(unique(solution))
    if (number_of_clusters != nclust) {
        metasnf_warning(
            "Spectral clustering provided a solution of size ", nclust,
            " when the number requested was 2."
        )
    }
    return(solution)
}

#' @rdname clust_fns
#' @export
spectral_three <- function(similarity_matrix) {
    number_of_clusters <- 3
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        number_of_clusters
    )
    nclust <- length(unique(solution))
    if (number_of_clusters != nclust) {
        metasnf_warning(
            "Spectral clustering provided a solution of size ", nclust,
            " when the number requested was 3."
        )
    }
    return(solution)
}

#' @rdname clust_fns
#' @export
spectral_four <- function(similarity_matrix) {
    number_of_clusters <- 4
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        number_of_clusters
    )
    nclust <- length(unique(solution))
    if (number_of_clusters != nclust) {
        metasnf_warning(
            "Spectral clustering provided a solution of size ", nclust,
            " when the number requested was 4."
        )
    }
    return(solution)
}

#' @rdname clust_fns
#' @export
spectral_five <- function(similarity_matrix) {
    number_of_clusters <- 5
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        number_of_clusters
    )
    nclust <- length(unique(solution))
    if (number_of_clusters != nclust) {
        metasnf_warning(
            "Spectral clustering provided a solution of size ", nclust,
            " when the number requested was 5."
        )
    }
    return(solution)
}

#' @rdname clust_fns
#' @export
spectral_six <- function(similarity_matrix) {
    number_of_clusters <- 6
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        number_of_clusters
    )
    nclust <- length(unique(solution))
    if (number_of_clusters != nclust) {
        metasnf_warning(
            "Spectral clustering provided a solution of size ", nclust,
            " when the number requested was 6."
        )
    }
    return(solution)
}

#' @rdname clust_fns
#' @export
spectral_seven <- function(similarity_matrix) {
    number_of_clusters <- 7
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        number_of_clusters
    )
    nclust <- length(unique(solution))
    if (number_of_clusters != nclust) {
        metasnf_warning(
            "Spectral clustering provided a solution of size ", nclust,
            " when the number requested was 7."
        )
    }
    return(solution)
}

#' @rdname clust_fns
#' @export
spectral_eight <- function(similarity_matrix) {
    number_of_clusters <- 8
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        number_of_clusters
    )
    nclust <- length(unique(solution))
    if (number_of_clusters != nclust) {
        metasnf_warning(
            "Spectral clustering provided a solution of size ", nclust,
            " when the number requested was 8."
        )
    }
    return(solution)
}

#' @rdname clust_fns
#' @export
spectral_nine <- function(similarity_matrix) {
    number_of_clusters <- 9
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        number_of_clusters
    )
    nclust <- length(unique(solution))
    if (number_of_clusters != nclust) {
        metasnf_warning(
            "Spectral clustering provided a solution of size ", nclust,
            " when the number requested was 9."
        )
    }
    return(solution)
}

#' @rdname clust_fns
#' @export
spectral_ten <- function(similarity_matrix) {
    number_of_clusters <- 10
    solution <- SNFtool::spectralClustering(
        similarity_matrix,
        number_of_clusters
    )
    nclust <- length(unique(solution))
    if (number_of_clusters != nclust) {
        metasnf_warning(
            "Spectral clustering provided a solution of size ", nclust,
            " when the number requested was 10."
        )
    }
    return(solution)
}
