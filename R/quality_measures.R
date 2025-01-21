#' Quality metrics
#'
#' These functions calculate conventional metrics of cluster solution quality.
#'
#' calculate_silhouettes: A wrapper for `cluster::silhouette` that calculates
#' silhouette scores for all cluster solutions in a provided solutions data
#' frame. Silhouette values range from -1 to +1 and indicate an overall ratio
#' of how close together observations within a cluster are to how far apart
#' observations across clusters are. You can learn more about interpreting
#' the results of this function by calling `?cluster::silhouette`.
#'
#' calculate_dunn_indices: A wrapper for `clv::clv.Dunn` that calculates
#' Dunn indices for all cluster solutions in a provided solutions data
#' frame. Dunn indices, like silhouette scores, similarly reflect similarity
#' within clusters and separation across clusters. You can learn more about
#' interpreting the results of this function by calling `?clv::clv.Dunn`.
#'
#' calculate_db_indices: A wrapper for `clv::clv.Davies.Bouldin` that
#' calculates Davies-Bouldin indices for all cluster solutions in a provided
#' solutions data frame. These values can be interpreted similarly as those
#' above. You can learn more about interpreting the results of this function by
#' calling `?clv::clv.Davies.Bouldin`.
#'
#' @param sol_df A `solutions_df` class object created by `batch_snf()` with
#'  the parameter `return_sim_mats = TRUE`.
#' @return A list of `silhouette` class objects, a vector of Dunn indices, or a
#'  vector of Davies-Bouldin indices depending on which function was used.
#' @examples
#' input_dl <- data_list(
#'     list(gender_df, "gender", "demographics", "categorical"),
#'     list(diagnosis_df, "diagnosis", "clinical", "categorical"),
#'     uid = "patient_id"
#' )
#' 
#' sc <- snf_config(input_dl, n_solutions = 5)
#' 
#' sol_df <- batch_snf(input_dl, sc, return_sim_mats = TRUE)
#' 
#' # calculate Davies-Bouldin indices
#' davies_bouldin_indices <- calculate_db_indices(sol_df)
#' 
#' # calculate Dunn indices
#' dunn_indices <- calculate_dunn_indices(sol_df)
#' 
#' # calculate silhouette scores
#' silhouette_scores <- calculate_silhouettes(sol_df)
#' @name quality_measures
NULL

#' @rdname quality_measures
#' @export
calculate_silhouettes <- function(sol_df) {
    similarity_matrices <- sim_mats_list(sol_df)
    if (all(sapply(similarity_matrices, is.null))) {
        metasnf_error(
            "Solutions data frame is missing similarity matrices attribute.",
            " Please set `return_sim_mats = TRUE` when calling `batch_snf()`."
        )
    }
    # Average out the intense signal present in the diagonals of the similarity
    #  matrices. Also, convert them into dissimilarity matrices by the logic
    #  of dissimilarity = max(similarity) - similarity.
    dissimilarity_matrices <- similarity_matrices |>
        lapply(
            function(similarity_matrix) {
                diag(similarity_matrix) <- mean(similarity_matrix)
                dissimilarity_matrix <- max(similarity_matrix) -
                    similarity_matrix
                return(dissimilarity_matrix)
            }
        )
    # Dataframe that contains patients along the rows, sol_df rows
    #  along the columns, and which cluster the patient was assigned to in the
    #  values.
    cluster_solutions_df <- t(sol_df)
    # cluster_solutions is a list of... cluster solutions. Each element in the
    #  list is a column from cluster_solutions_df, excluding the uid
    #  column.
    cluster_solutions <- sapply(
        cluster_solutions_df[, -1],
        function(column) {
            list(column)
        }
    )
    silhouette_scores <- Map(
        function(cluster_solution, dissimilarity_matrix) {
            # Note: the cluster package should not be converted to an optional
            #  package in "Suggests". cluster::daisy is a default distance
            #  measure required for categorical and mixed data.
            silhouette_score <- cluster::silhouette(
                x = as.integer(cluster_solution),
                dmatrix = dissimilarity_matrix
            )
            return(silhouette_score)
        },
        cluster_solutions,
        dissimilarity_matrices
    )
    return(silhouette_scores)
}

#' @rdname quality_measures
#' @export
calculate_dunn_indices <- function(sol_df) {
    if (!requireNamespace("clv", quietly = TRUE)) {
        metasnf_error(
            "Package \"clv\" must be installed to use this function."
        )
    }
    similarity_matrices <- attributes(sol_df)$"sim_mats_list"
    all_is_null <- lapply(
        similarity_matrices,
        function(x) {
            is.null(x)
        }
    ) |>
        unlist() |>
        all()
    if (all_is_null) {
        metasnf_error(
            "Solutions data frame is missing similarity matrices attribute.",
            " Please set `return_sim_mats = TRUE` when calling `batch_snf()`."
        )
    }
    # Average out the intense signal present in the diagonals of the similarity
    #  matrices. Also, convert them into dissimilarity matrices by the logic
    #  of dissimilarity = max(similarity) - similarity.
    dissimilarity_matrices <- similarity_matrices |>
        lapply(
            function(similarity_matrix) {
                diag(similarity_matrix) <- mean(similarity_matrix)
                dissimilarity_matrix <- max(similarity_matrix) -
                    similarity_matrix
                return(dissimilarity_matrix)
            }
        )
    # Dataframe that contains patients along the rows, sol_df rows
    #  along the columns, and which cluster the patient was assigned to in the
    #  values.
    cluster_solutions_df <- t(sol_df)
    # cluster_solutions is a list of... cluster solutions. Each element in the
    #  list is a column from cluster_solutions_df, excluding the uid
    #  column.
    cluster_solutions <- sapply(
        cluster_solutions_df[, -1],
        function(column) {
            list(column)
        }
    )
    dunn_indices <- Map(
        function(cluster_solution, dissimilarity_matrix) {
            # Vector of solutions must be in integer form to use
            #  cls.scatt.diss.mx
            cluster_solution <- as.integer(cluster_solution)
            # The cls.scatt.diss.mx takes in a dissimilarity matrix and returns
            #  an object storing popular inter and intracluster distances. This
            #  object is referred to in clv documentation as the index.list, so
            #  that name is used here.
            index_list <- clv::cls.scatt.diss.mx(
                diss.mx = dissimilarity_matrix,
                clust = cluster_solution
            )
            dunn_index <- clv::clv.Dunn(
                index.list = index_list,
                # the intracluster distance methods to evaluate
                intracls = c(
                    "complete",
                    "average"
                ),
                # the intercluster distance methods to evaluate
                intercls = c(
                    "single",
                    "complete",
                    "average",
                    "hausdorff"
                )
            )
            return(dunn_index)
        },
        cluster_solutions,
        dissimilarity_matrices
    )
    return(dunn_indices)
}

#' @rdname quality_measures
#' @export
calculate_db_indices <- function(sol_df) {
    if (!requireNamespace("clv", quietly = TRUE)) {
        metasnf_error(
            "Package \"clv\" must be installed to use this function."
        )
    }
    similarity_matrices <- attributes(sol_df)$"sim_mats_list"
    all_is_null <- lapply(
        similarity_matrices,
        function(x) {
            is.null(x)
        }
    ) |>
        unlist() |>
        all()
    if (all_is_null) {
        metasnf_error(
            "Solutions data frame is missing similarity matrices attribute.",
            " Please set `return_sim_mats = TRUE` when calling `batch_snf()`."
        )
    }
    # Average out the intense signal present in the diagonals of the similarity
    #  matrices. Also, convert them into dissimilarity matrices by the logic
    #  of dissimilarity = max(similarity) - similarity.
    dissimilarity_matrices <- similarity_matrices |>
        lapply(
            function(similarity_matrix) {
                diag(similarity_matrix) <- mean(similarity_matrix)
                dissimilarity_matrix <- max(similarity_matrix) -
                    similarity_matrix
                return(dissimilarity_matrix)
            }
        )
    # Dataframe that contains patients along the rows, sol_df rows
    #  along the columns, and which cluster the patient was assigned to in the
    #  values.
    cluster_solutions_df <- t(sol_df)
    # cluster_solutions is a list of... cluster solutions. Each element in the
    #  list is a column from cluster_solutions_df, excluding the uid
    #  column.
    cluster_solutions <- sapply(
        cluster_solutions_df[, -1],
        function(column) {
            list(column)
        }
    )
    davies_bouldin_indices <- Map(
        function(cluster_solution, dissimilarity_matrix) {
            # Vector of solutions must be in integer form to use
            #  cls.scatt.diss.mx
            cluster_solution <- as.integer(cluster_solution)
            # The cls.scatt.diss.mx takes in a dissimilarity matrix and returns
            #  an object storing popular inter and intracluster distances. This
            #  object is referred to in clv documentation as the index.list, so
            #  that name is used here.
            index_list <- clv::cls.scatt.diss.mx(
                diss.mx = dissimilarity_matrix,
                clust = cluster_solution
            )
            davies_bouldin_index <- clv::clv.Davies.Bouldin(
                index.list = index_list,
                # the intracluster distance methods to evaluate
                intracls = c(
                    "complete",
                    "average"
                ),
                # the intercluster distance methods to evaluate
                intercls = c(
                    "single",
                    "complete",
                    "average",
                    "hausdorff"
                )
            )
            return(davies_bouldin_index)
        },
        cluster_solutions,
        dissimilarity_matrices
    )
    return(davies_bouldin_indices)
}
