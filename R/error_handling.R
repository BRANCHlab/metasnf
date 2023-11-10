#' Returns formatted string representation of current time
current_time <- function() {
    time <- Sys.time()
    time <- gsub("-", "_", time)
    time <- gsub(" ", "_", time)
    time <- gsub(":", "_", time)
    time <- gsub("EST", "", time)
    time <- sub(".[^\\.]+$", "", time, fixed = FALSE)
    return(time)
}

#' Error handling-equipped estimation of the number of clusters in a graph
#'
#' Wrapper for SNFtool::estimateNumberOfClustersGivenGraph which includes a
#'  tryCatch statement to identify errors. Error-raising similarity matrices
#'  get saved to file for debugging.
#'
#' @param similarity_matrix A similarity matrix.
#'
#' @export
try_estimating_clusters <- function(similarity_matrix) {
    tryCatch(
        {
            estimated_n <- SNFtool::estimateNumberOfClustersGivenGraph(
                similarity_matrix
            )
            return(estimated_n)
        },
        error = function(cond) {
            sm_name <- paste0(current_time(), "_similarity_matrix.csv")
            write_csv(similarity_matrix, sm_name)
            stop(
                "An error has occurred during a call to",
                " SNFtool::estimateNumberOfClustersGivenGraph(). The",
                " similarity matrix that resulted in the error is being",
                " written as: ", sm_name
            )
        }
    )
}

#' Error handling-equipped estimation of the number of clusters in a graph
#'
#' Wrapper for SNFtool::estimateNumberOfClustersGivenGraph which includes a
#'  tryCatch statement to identify errors. Error-raising similarity matrices
#'  get saved to file for debugging.
#'
#' @param similarity_matrix A similarity matrix.
#'
#' @export
try_spectral_clustering <- function(similarity_matrix, number_of_clusters) {
    tryCatch(
        {
            solution <- SNFtool::spectralClustering(
                similarity_matrix,
                number_of_clusters
            )
            return(solution)
        },
        error = function(cond) {
            sm_name <- paste0(current_time(), "_similarity_matrix.csv")
            write_csv(similarity_matrix, sm_name)
            stop(
                "An error has occurred during a call to",
                " SNFtool::spectralClustering(). The similarity",
                " matrix that resulted in the error is being written as: ",
                sm_name, ". The number of clusters that resulted in this",
                " error is ", number_of_clusters, "."
            )
        }
    )
}
