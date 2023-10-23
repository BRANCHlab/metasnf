#' Calculate silhouette scores
#'
#' Given a solutions_matrix and a list of affinity_matrices (or a single
#'  affinity_matrix if the solutions_matrix has only 1 row), return a list
#'  of 'silhouette' objects from the cluster package
#'
#' @param solutions_matrix A solutions_matrix (see ?batch_snf)
#' @param affinity_matrices A list of affinity matrices (see ?batch_snf)
#'
#' @return silhouette_scores A list of "silhouette" objects from the cluster
#'  package.
#'
#' @examples
#'
#' # load package
#' library(metasnf)
#'
#' # generate data_list
#' data_list <- generate_data_list(
#'     list(abcd_cort_t, "cortical_thickness", "neuroimaging", "continuous"),
#'     list(abcd_cort_sa, "cortical_surface_area", "neuroimaging", "continuous"),
#'     list(abcd_subc_v, "subcortical_volume", "neuroimaging", "continuous"),
#'     list(abcd_income, "household_income", "demographics", "continuous"),
#'     list(abcd_pubertal, "pubertal_status", "demographics", "continuous"),
#'     uid = "patient"
#' )
#'
#' # build settings_matrix
#' settings_matrix <- generate_settings_matrix(data_list, nrow = 15, seed = 42)
#'
#' # collect affinity matrices and solutions matrix from batch_snf
#' batch_snf_results <- batch_snf(
#'     data_list,
#'     settings_matrix,
#'     return_affinity_matrices = TRUE
#' )
#'
#' solutions_matrix <- batch_snf_results$"solutions_matrix"
#' affinity_matrices <- batch_snf_results$"affinity_matrices"
#'
#' # calculate silhouette scores
#' silhouette_scores <- calculate_silhouettes(
#'     solutions_matrix,
#'     affinity_matrices
#' )
#'
#' # plot the silhouette scores of the first solutions
#' plot(silhouette_scores[[1]])
#'
#' @export
calculate_silhouettes <- function(solutions_matrix, affinity_matrices) {
    # The size of the solutions_matrix and the number of affinity_matrices
    #  should match up. First, handle the special case of the user providing
    #  a single affinity_matrix not bundled in a list.
    if (inherits(affinity_matrices, "matrix")) {
        affinity_matrices <- list(affinity_matrices)
    }
    # Then ensure the size of the two arguments align.
    if (nrow(solutions_matrix) != length(affinity_matrices)) {
        stop(
            paste0(
                "Size of solutions_matrix does not match length of",
                " affinity_matrices."
            )
        )
    }
    # Average out the intense signal present in the diagonals of the affinity
    #  matrices. Also, convert them into dissimilarity matrices by the logic
    #  of dissimilarity = max(similarity) - similarity.
    dissimilarity_matrices <- affinity_matrices |>
        lapply(
            function(affinity_matrix) {
                diag(affinity_matrix) <- mean(affinity_matrix)
                dissimilarity_matrix <- max(affinity_matrix) - affinity_matrix
                return(dissimilarity_matrix)
            }
        )
    # Dataframe that contains patients along the rows, solutions_matrix rows
    #  along the columns, and which cluster the patient was assigned to in the
    #  values.
    cluster_solutions_df <- get_cluster_solutions(solutions_matrix)
    # cluster_solutions is a list of... cluster solutions. Each element in the
    #  list is a column from cluster_solutions_df, excluding the subjectkey
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
                x = cluster_solution,
                dmatrix = dissimilarity_matrix
            )
            return(silhouette_score)
        },
        cluster_solutions,
        dissimilarity_matrices
    )
    return(silhouette_scores)
}

#' Calculate Dunn indices
#'
#' Given a solutions_matrix and a list of affinity_matrices (or a single
#'  affinity_matrix if the solutions_matrix has only 1 row), return a vector of
#'  Dunn indices
#'
#' @param solutions_matrix A solutions_matrix (see ?batch_snf)
#' @param affinity_matrices A list of affinity matrices (see ?batch_snf)
#'
#' @return dunn_indices A vector of Dunn indices for each cluster solution
#'
#' @examples
#'
#' if (require("clv")) {
#'     # load package
#'     library(metasnf)
#'
#'     # generate data_list
#'     data_list <- generate_data_list(
#'         list(abcd_cort_t, "cortical_thickness", "neuroimaging", "continuous"),
#'         list(abcd_cort_sa, "cortical_surface_area", "neuroimaging", "continuous"),
#'         list(abcd_subc_v, "subcortical_volume", "neuroimaging", "continuous"),
#'         list(abcd_income, "household_income", "demographics", "continuous"),
#'         list(abcd_pubertal, "pubertal_status", "demographics", "continuous"),
#'         uid = "patient"
#'     )
#'
#'     # build settings_matrix
#'     settings_matrix <- generate_settings_matrix(data_list, nrow = 15, seed = 42)
#'
#'     # collect affinity matrices and solutions matrix from batch_snf
#'     batch_snf_results <- batch_snf(
#'         data_list,
#'         settings_matrix,
#'         return_affinity_matrices = TRUE
#'     )
#'
#'     solutions_matrix <- batch_snf_results$"solutions_matrix"
#'     affinity_matrices <- batch_snf_results$"affinity_matrices"
#'
#'     # calculate Dunn indices
#'     dunn_indices <- calculate_dunn_indices(
#'         solutions_matrix,
#'         affinity_matrices
#'     )
#' }
#'
#' @export
calculate_dunn_indices <- function(solutions_matrix, affinity_matrices) {
    if (!requireNamespace("clv", quietly = TRUE)) {
        stop(
            "Package \"clv\" must be installed to use this function.",
            call. = FALSE
        )
    }
    # The size of the solutions_matrix and the number of affinity_matrices
    #  should match up. First, handle the special case of the user providing
    #  a single affinity_matrix not bundled in a list.
    if (inherits(affinity_matrices, "matrix")) {
        affinity_matrices <- list(affinity_matrices)
    }
    # Then ensure the size of the two arguments align.
    if (nrow(solutions_matrix) != length(affinity_matrices)) {
        stop(
            paste0(
                "Size of solutions_matrix does not match length of",
                " affinity_matrices."
            )
        )
    }
    # Average out the intense signal present in the diagonals of the affinity
    #  matrices. Also, convert them into dissimilarity matrices by the logic
    #  of dissimilarity = max(similarity) - similarity.
    dissimilarity_matrices <- affinity_matrices |>
        lapply(
            function(affinity_matrix) {
                diag(affinity_matrix) <- mean(affinity_matrix)
                dissimilarity_matrix <- max(affinity_matrix) - affinity_matrix
                return(dissimilarity_matrix)
            }
        )
    # Dataframe that contains patients along the rows, solutions_matrix rows
    #  along the columns, and which cluster the patient was assigned to in the
    #  values.
    cluster_solutions_df <- get_cluster_solutions(solutions_matrix)
    # cluster_solutions is a list of... cluster solutions. Each element in the
    #  list is a column from cluster_solutions_df, excluding the subjectkey
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

#' Calculate Davies-Bouldin indices
#'
#' Given a solutions_matrix and a list of affinity_matrices (or a single
#'  affinity_matrix if the solutions_matrix has only 1 row), return a vector of
#'  Davies-Bouldin indices
#'
#' @param solutions_matrix A solutions_matrix (see ?batch_snf)
#' @param affinity_matrices A list of affinity matrices (see ?batch_snf)
#'
#' @return davies_bouldin_indices A vector of Davies-Bouldin indices for each
#'  cluster solution
#'
#' @examples
#'
#' if (require("clv")) {
#'     # load package
#'     library(metasnf)
#'
#'     # generate data_list
#'     data_list <- generate_data_list(
#'         list(abcd_cort_t, "cortical_thickness", "neuroimaging", "continuous"),
#'         list(abcd_cort_sa, "cortical_surface_area", "neuroimaging", "continuous"),
#'         list(abcd_subc_v, "subcortical_volume", "neuroimaging", "continuous"),
#'         list(abcd_income, "household_income", "demographics", "continuous"),
#'         list(abcd_pubertal, "pubertal_status", "demographics", "continuous"),
#'         uid = "patient"
#'     )
#'
#'     # build settings_matrix
#'     settings_matrix <- generate_settings_matrix(data_list, nrow = 15, seed = 42)
#'
#'     # collect affinity matrices and solutions matrix from batch_snf
#'     batch_snf_results <- batch_snf(
#'         data_list,
#'         settings_matrix,
#'         return_affinity_matrices = TRUE
#'     )
#'
#'     solutions_matrix <- batch_snf_results$"solutions_matrix"
#'     affinity_matrices <- batch_snf_results$"affinity_matrices"
#'
#'     # calculate Davies-Bouldin indices
#'     davies_bouldin_indices <- calculate_db_indices(
#'         solutions_matrix,
#'         affinity_matrices
#'     )
#' }
#' @export
calculate_db_indices <- function(solutions_matrix, affinity_matrices) {
    if (!requireNamespace("clv", quietly = TRUE)) {
        stop(
            "Package \"clv\" must be installed to use this function.",
            call. = FALSE
        )
    }
    # The size of the solutions_matrix and the number of affinity_matrices
    #  should match up. First, handle the special case of the user providing
    #  a single affinity_matrix not bundled in a list.
    if (inherits(affinity_matrices, "matrix")) {
        affinity_matrices <- list(affinity_matrices)
    }
    # Then ensure the size of the two arguments align.
    if (nrow(solutions_matrix) != length(affinity_matrices)) {
        stop(
            paste0(
                "Size of solutions_matrix does not match length of",
                " affinity_matrices."
            )
        )
    }
    # Average out the intense signal present in the diagonals of the affinity
    #  matrices. Also, convert them into dissimilarity matrices by the logic
    #  of dissimilarity = max(similarity) - similarity.
    dissimilarity_matrices <- affinity_matrices |>
        lapply(
            function(affinity_matrix) {
                diag(affinity_matrix) <- mean(affinity_matrix)
                dissimilarity_matrix <- max(affinity_matrix) - affinity_matrix
                return(dissimilarity_matrix)
            }
        )
    # Dataframe that contains patients along the rows, solutions_matrix rows
    #  along the columns, and which cluster the patient was assigned to in the
    #  values.
    cluster_solutions_df <- get_cluster_solutions(solutions_matrix)
    # cluster_solutions is a list of... cluster solutions. Each element in the
    #  list is a column from cluster_solutions_df, excluding the subjectkey
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
