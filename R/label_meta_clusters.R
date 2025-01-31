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
#' @export
#' @examples
#' #dl <- data_list(
#' #    list(cort_sa, "cortical_surface_area", "neuroimaging", "continuous"),
#' #    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
#' #    list(income, "household_income", "demographics", "continuous"),
#' #    list(pubertal, "pubertal_status", "demographics", "continuous"),
#' #    uid = "unique_id"
#' #)
#' #
#' #set.seed(42)
#' #my_sc <- snf_config(
#' #    dl = dl,
#' #    n_solutions = 20,
#' #    min_k = 20,
#' #    max_k = 50
#' #)
#' #
#' #sol_df <- batch_snf(dl, my_sc)
#' #
#' #sol_df
#' #
#' #sol_aris <- calc_aris(sol_df)
#' #
#' #meta_cluster_order <- get_matrix_order(sol_aris)
#' #
#' ## `split_vec` found by iteratively plotting ari_hm or by ?shiny_annotator()
#' #split_vec <- c(6, 10, 16)
#' #ari_hm <- meta_cluster_heatmap(
#' #    sol_aris,
#' #    order = meta_cluster_order,
#' #    split_vector = split_vec
#' #)
#' #
#' #mc_sol_df <- label_meta_clusters(
#' #    sol_df,
#' #    order = meta_cluster_order,
#' #    split_vector = split_vec
#' #)
#' #
#' #mc_sol_df
label_meta_clusters <- function(sol_df, split_vector, order = NULL) {
    n_solutions <- nrow(sol_df)
    # label splits ------------------------------------------------------------
    labels <- rep("A", n_solutions)
    if (split_vector[length(split_vector)] != n_solutions) {
        split_vector <- c(split_vector, n_solutions)
    }
    for (i in 1:(length(split_vector) - 1)) {
        start <- split_vector[i]
        end <- split_vector[i + 1]
        labels[start:end] <- LETTERS[i + 1]
    }
    #--------------------------------------------------------------------------
    if (is.null(order)) {
        order <- seq_len(nrow(sol_df))
    }
    sol_df$"mc" <- labels[order(order)]
    sol_df <- sol_df[, unique(c("solution", "nclust", "mc", colnames(sol_df)))]
    return(sol_df)
}
