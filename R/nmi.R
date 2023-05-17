#' Calculate NMI scores for SNF inputs
#'
#' @param om_row An output matrix row
#' @param data_list A data list
#'
#' @return nmi_df A dataframe containing input NMI scores in descending order
#'
#' @export
calc_nmi <- function(om_row, data_list) {
    K <- om_row$"K"
    alpha <- om_row$"alpha"
    nclust <- om_row$"nclust"
    # Getting the clustering results of the full fused network
    full_solution <- as.numeric(subs(om_row)[, -1])
    # Getting the clustering results of each individual SNF input
    dist_list <- lapply(data_list,
        function(x) {
            get_dist_matrix(df = x$"data", input_type = x$"type")
        })
    sim_list <- lapply(dist_list,
        function(x) {
            SNFtool::affinityMatrix(x, K = K, sigma = alpha)
        })
    input_solutions <- lapply(sim_list,
        function(x) {
            tryCatch({
                SNFtool::spectralClustering(x, nclust)
            },
            error = function(cond) {
                spectral_clustering(x, nclust)
            }
            )
        })
    nmi_scores <- lapply(input_solutions,
        function(x) {
            SNFtool::calNMI(full_solution, x)
        })
    nmi_scores <- format(signif(unlist(nmi_scores), 2), scientific = FALSE)
    nmi_df <- data.frame(
        input = summarize_dl(data_list)$"name",
        nmi = nmi_scores)
    nmi_df <- nmi_df |> dplyr::arrange(dplyr::desc(nmi_df$"nmi"))
    return(nmi_df)
}

#' Calculate NMI scores for an output matrix
#'
#' Given an output matrix in dataframe form with "significance" columns,
#'  calculate NMIs for all inputs and combine results into a single dataframe
#'
#' @param om Output matrix
#' @param data_list A data list
#'
#' @return nmi_df Merged dataframe of all NMI values
#'
#' @export
om_to_nmi_df <- function(om, data_list) {
    for (row in seq_len(nrow(om))) {
        if (row == 1) {
            nmi_df <- calc_nmi(om[row, ], data_list)
            colnames(nmi_df) <- c("input", om$"significance"[row])
        } else {
            current_df <- calc_nmi(om[row, ], data_list)
            colnames(current_df) <- c("input", om$"significance"[row])
            nmi_df <- dplyr::inner_join(nmi_df, current_df, by = "input")
        }
    }
    return(nmi_df)
}
