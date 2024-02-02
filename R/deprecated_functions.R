#' (DEPRECATED) Function for label propagating results in a solutions_matrix
#'
#' @param solutions_matrix A solutions_matrix (training set only)
#' @param full_data_list A data_list containing training and testing subjects
#' @param clust_algs_list The clustering algorithms list used to create the
#' original solutions matrix (if any was used)
#' @param distance_metrics_list The distance metrics list used to create the
#' original solutions matrix (if any was used)
#' @param weights_matrix The weights matrix used to create the original
#' solutions matrix (if any was used)
#'
#' @export
lp_row <- function(solutions_matrix,
                   full_data_list,
                   clust_algs_list = NULL,
                   distance_metrics_list = NULL,
                   weights_matrix = NULL) {
    stop("This function is deprecated. Use lp_solutions_matrix instead.")
}
