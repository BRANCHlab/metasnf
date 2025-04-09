#' TITLE
#'
#' SHORT DESCRIPTION
#'
#' @param PARAM1
#'
#' @return RETURN
#'
#' @export
plot.solutions_df <- function(x,
                              y = NULL,
                              cluster_rows = FALSE,
                              cluster_columns = TRUE,
                              heatmap_legend_param = NULL,
                              row_title = "Solution",
                              column_title = "Observation",
                              ...) {
    if (is.null(heatmap_legend_param)) {
        heatmap_legend_param <- list(
            title = "Cluster"
        )
    }
    sol_mat <- x |>
        as.data.frame() |>
        dplyr::select(dplyr::starts_with("uid")) |>
        as.matrix()
    colnames(sol_mat) <- gsub("^uid_", "", colnames(sol_mat))
    colours <- RColorBrewer::brewer.pal(max(sol_mat), "Set3")
    names(colours) <- seq_len(max(sol_mat))
    ComplexHeatmap::Heatmap(
        sol_mat,
        cluster_rows = cluster_rows,
        cluster_columns = cluster_columns,
        col = colours,
        heatmap_legend_param = heatmap_legend_param,
        row_title = row_title,
        column_title = column_title,
        ...
    )
}
