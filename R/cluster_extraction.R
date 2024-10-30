#' Extract cluster membership information from a solutions_matrix
#'
#' This function takes in a solutions matrix and returns a dataframe containing
#' the cluster assignments for each subjectkey. It is similar to
#' '`get_clusters()`, which takes one solutions matrix row and returns a vector
#' of cluster assignments' and `get_cluster_df()`, which takes a solutions
#' matrix with only one row and returns a dataframe with two columns: "cluster"
#' and "subjectkey" (the UID of the observation).
#'
#' @param solutions_matrix A solutions_matrix.
#'
#' @return cluster_solutions A "data.frame" object where each row is an
#' observation and each column (apart from the subjectkey column) indicates
#' the cluster that observation was assigned to for the corresponding
#' solutions matrix row.
#'
#' @export
get_cluster_solutions <- function(solutions_matrix) {
    # Create a skeleton dataframe using just the columns of the solutions
    # matrix containing information about which cluster each patient was
    # assigned to on each SNF run
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

#' Extract cluster membership information from one solutions matrix row
#'
#' This function takes in a single row of a solutions matrix and returns a
#' dataframe containing the cluster assignments for each subjectkey. It is
#' similar to `get_clusters()`, which takes one solutions matrix row and
#' returns a vector of cluster assignments' and `get_cluster_solutions()`,
#' which takes a solutions matrix with any number of rows and returns a
#' dataframe indicating the cluster assignments for each of those rows.
#'
#' @param solutions_matrix_row One row from a solutions matrix.
#'
#' @return cluster_df dataframe of cluster and subjectkey.
#'
#' @export
get_cluster_df <- function(solutions_matrix_row) {
    cluster_df <-
        subs(solutions_matrix_row) |>
        t() |>
        data.frame()
    cluster_df$id <- rownames(cluster_df)
    colnames(cluster_df) <- c("cluster", "subjectkey")
    cluster_df <- cluster_df[2:nrow(cluster_df), ]
    cluster_df <- cluster_df |>
        dplyr::select("subjectkey", "cluster")
    rownames(cluster_df) <- NULL
    return(cluster_df)
}

#' Extract cluster membership vector from one solutions matrix row
#'
#' This function takes in a single row of a solutions matrix and returns a
#' vector containing the cluster assignments for each observation. It is
#' similar to `get_cluster_df()`, which takes a solutions matrix with only one
#' row and returns a dataframe with two columns: "cluster" and "subjectkey"
#' '(the UID of the observation) and `get_cluster_solutions()`, which takes a
#' solutions matrix with any number of rows and returns a dataframe indicating
#' the cluster assignments for each of those rows.
#'
#' @param solutions_matrix_row Output matrix row.
#'
#' @return clusters Vector of assigned clusters.
#'
#' @export
get_clusters <- function(solutions_matrix_row) {
    cluster_df <-
        subs(solutions_matrix_row) |>
        t() |>
        data.frame()
    cluster_df$id <- rownames(cluster_df)
    rownames(cluster_df) <- NULL
    colnames(cluster_df) <- c("cluster", "subjectkey")
    cluster_df <- cluster_df[2:nrow(cluster_df), ]
    clusters <- cluster_df$"cluster"
    return(clusters)
}
