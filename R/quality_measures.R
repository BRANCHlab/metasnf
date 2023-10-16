#' Calculate silhouette scores
#'
#' Given a solutions_matrix and a list of affinity_matrices (or a single
#'  affinity_matrix if the solutions_matrix has only 1 row), return a list
#'
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
#'     list(abcd_cort_t, "cortical_thickness", "neuroimaging", "numeric"),
#'     list(abcd_cort_sa, "cortical_surface_area", "neuroimaging", "numeric"),
#'     list(abcd_subc_v, "subcortical_volume", "neuroimaging", "numeric"),
#'     list(abcd_income, "household_income", "demographics", "numeric"),
#'     list(abcd_pubertal, "pubertal_status", "demographics", "numeric"),
#'     old_uid = "patient"
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
            cluster::silhouette(
                x = cluster_solution,
                dmatrix = dissimilarity_matrix
            )
        },
        cluster_solutions,
        dissimilarity_matrices
    )
    return(silhouette_scores)
}
