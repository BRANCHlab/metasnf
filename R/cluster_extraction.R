#' Extract cluster membership information from a sol_df
#'
#' This function takes in a solutions data frame and returns a dataframe containing
#' the cluster assignments for each uid. It is similar to
#' '`get_clusters()`, which takes one solutions data frame row and returns a vector
#' of cluster assignments' and `get_cluster_df()`, which takes a solutions
#' matrix with only one row and returns a dataframe with two columns: "cluster"
#' and "uid" (the UID of the observation).
#'
#' @param sol_df A sol_df.
#' @return cluster_solutions A "data.frame" object where each row is an
#'  observation and each column (apart from the uid column) indicates
#'  the cluster that observation was assigned to for the corresponding
#'  solutions data frame row.
#' @export
get_cluster_solutions <- function(sol_df) {
    class(sol_df) <- "data.frame"
    # Create a skeleton dataframe using just the columns of the solutions
    # matrix containing information about which cluster each patient was
    # assigned to on each SNF run
    cluster_solutions <- t(sol_df)
    # Assign the column names to match the corresponding SNF run
    colnames(cluster_solutions) <- rownames(sol_df)
    # Remove the first row, which just contains the solution. That info is now
    #  in the column names.
    cluster_solutions <- cluster_solutions[-1, , drop = FALSE]
    # Store the uids of the observations in a separate dataframe
    subjects_df <- data.frame(rownames(cluster_solutions))
    colnames(subjects_df) <- "uid"
    # Append that subject dataframe to the full cluster solution (preserving
    #  the info without relying on rownames)
    cluster_solutions <- cbind(subjects_df, cluster_solutions)
    # Remove the superfluous rownames
    rownames(cluster_solutions) <- NULL
    return(cluster_solutions)
}

#' Extract cluster membership information from one solutions data frame row
#'
#' This function takes in a single row of a solutions data frame and returns a
#' dataframe containing the cluster assignments for each uid. It is
#' similar to `get_clusters()`, which takes one solutions data frame row and
#' returns a vector of cluster assignments' and `get_cluster_solutions()`,
#' which takes a solutions data frame with any number of rows and returns a
#' dataframe indicating the cluster assignments for each of those rows.
#'
#' @param sol_df_row One row from a solutions data frame.
#'
#' @return cluster_df dataframe of cluster and uid.
#'
#' @export
get_cluster_df <- function(sol_df_row) {
    class(sol_df_row) <- "data.frame"
    cluster_df <-
        uids(sol_df_row) |>
        t() |>
        data.frame()
    cluster_df$id <- rownames(cluster_df)
    colnames(cluster_df) <- c("cluster", "uid")
    cluster_df <- cluster_df[2:nrow(cluster_df), ]
    cluster_df <- cluster_df |>
        dplyr::select("uid", "cluster")
    rownames(cluster_df) <- NULL
    return(cluster_df)
}

#' Extract cluster membership vector from one solutions data frame row
#'
#' This function takes in a single row of a solutions data frame and returns a
#' vector containing the cluster assignments for each observation. It is
#' similar to `get_cluster_df()`, which takes a solutions data frame with only one
#' row and returns a dataframe with two columns: "cluster" and "uid"
#' '(the UID of the observation) and `get_cluster_solutions()`, which takes a
#' solutions data frame with any number of rows and returns a dataframe indicating
#' the cluster assignments for each of those rows.
#'
#' @param sol_df_row Output matrix row.
#'
#' @return clusters Vector of assigned clusters.
#'
#' @export
get_clusters <- function(sol_df_row) {
    class(sol_df_row) <- "data.frame"
    cluster_df <-
        subs(sol_df_row) |>
        t() |>
        data.frame()
    cluster_df$id <- rownames(cluster_df)
    rownames(cluster_df) <- NULL
    colnames(cluster_df) <- c("cluster", "uid")
    cluster_df <- cluster_df[2:nrow(cluster_df), ]
    clusters <- cluster_df$"cluster"
    return(clusters)
}
