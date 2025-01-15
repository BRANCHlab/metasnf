#' Assign meta cluster labels to rows of a solutions data frame or extended
#' solutions data frame
#'
#' Given a solutions data frame or extended solutions data frame class object
#' and a numeric vector indicating which rows correspond to which meta
#' clusters, assigns meta clustering information to the "meta_clusters"
#' attribute of the data frame.
#'
#' @param sol_df A solutions data frame or extended solutions data frame to
#'  assign meta clusters to.
#' @param split_vector A numeric vector indicating which rows of sol_df should
#'  be the split points for meta cluster labeling.
#' @param order An optional numeric vector indicating how the solutions data 
#'  frame should be reordered prior to meta cluster labeling. This vector can
#'  be obtained by running `get_matrix_order()` on an ARI matrix, which itself
#'  can be obtained by calling `calc_aris()` on a solutions data frame.
#' @return A solutions data frame with a populated "meta_clusters" attribute.
#'
#' @export
label_meta_clusters <- function(sol_df, split_vector, order = NULL) {
    n_solutions <- nrow(sol_df)
    labels <- rep("A", n_solutions)
    if (split_vector[length(split_vector)] != n_solutions) {
        split_vector <- c(split_vector, n_solutions)
    }
    for (i in 1:(length(split_vector) - 1)) {
        start <- split_vector[i]
        end <- split_vector[i + 1]
        labels[start:end] <- LETTERS[i + 1]
    }
    if (is.null(order)) {
        order <- seq_len(nrow(sol_df))
    }
    sol_df_ordered <- sol_df[order, ]
    corresponding_solutions <- sol_df_ordered$"solution"
    sol_df_ordered$"label" <- labels
    sol_df_ordered <- sol_df_ordered |>
        dplyr::select(solution, nclust, label, dplyr::everything())
    return(sol_df_ordered)
    print(corresponding_solutions)
    print(labels)
    return(labels[order])
}
