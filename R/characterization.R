#' Extract all cluster solutions from a solutions_matrix
#'
#' @param solutions_matrix A solutions_matrix.
#'
#' @return cluster_solutions A dataframe where each row is a patient and each
#'  column is a different run of SNF stored in the solutions_matrix. Values
#'  along the columns are the cluster that each patient was assigned to.
#'
#' @export
get_cluster_solutions <- function(solutions_matrix) {
    # Create a skeleton dataframe using just the columns of the solutions
    #  matrix containing information about which cluster each patient was
    #  assigned to on each SNF run
    cluster_solutions <- solutions_matrix |>
        subs() |>
        t() |>
        data.frame()
    # Assign the column names to match the corresponding SNF run
    colnames(cluster_solutions) <- rownames(solutions_matrix)
    # Remove the first row, which just contains the row_id. That info is now
    #  in the column names.
    cluster_solutions <- cluster_solutions[-1, , drop = FALSE]
    # Store the subjectkeys of the observations in a separate dataframe
    subjects_df <- data.frame(rownames(cluster_solutions))
    colnames(subjects_df) <- "subjectkey"
    # Append that subject dataframe to the full cluster solution (preserving
    #  the info without relying on rownames)
    cluster_solutions <- cbind(subjects_df, cluster_solutions)
    # Remove the superfluous rownames
    rownames(cluster_solutions) <- NULL
    return(cluster_solutions)
}

#' Extract dataframe of cluster and subject key from solutions matrix row
#'
#' @param om_row Output matrix row
#'
#' @return cluster_df dataframe of cluster and subjectkey
#'
#' @export
get_cluster_df <- function(om_row) {
    cluster_df <-
        subs(om_row) |>
        t() |>
        data.frame()
    cluster_df$id <- rownames(cluster_df)
    rownames(cluster_df) <- NULL
    colnames(cluster_df) <- c("cluster", "subjectkey")
    cluster_df <- cluster_df[2:nrow(cluster_df), ]
    return(cluster_df)
}

#' Extract list of assigned clusters
#'
#' @param om_row Output matrix row
#'
#' @return clusters list of assigned clusters
#'
#' @export
get_clusters <- function(om_row) {
    cluster_df <-
        subs(om_row) |>
        t() |>
        data.frame()
    cluster_df$id <- rownames(cluster_df)
    rownames(cluster_df) <- NULL
    colnames(cluster_df) <- c("cluster", "subjectkey")
    cluster_df <- cluster_df[2:nrow(cluster_df), ]
    clusters <- cluster_df$"cluster"
    return(clusters)
}
