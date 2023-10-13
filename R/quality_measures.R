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
            function(x) {
                diag(x) <- mean(x)
                dissimilarity <- max(x) - x
                return(dissimilarity)
            }
        )
    return(dissimilarity_matrices)
}
