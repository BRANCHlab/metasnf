#' Plot of cluster assignments in a solutions data frame
#'
#' This plot, built on `ComplexHeatmap::Heatmap()`, visualizes the cluster
#' assignments in a solutions data frame as a categorical heatmap with
#' observations along the columns and clusters along the rows.
#'
#' @inheritParams ComplexHeatmap::Heatmap
#' @param x A `solutions_df` object.
#' @param y Optional argument to `plot`, not used in this method.
#' @param ... Additional arguments passed to `ComplexHeatmap::Heatmap()`.
#' @return A `ComplexHeatmap::Heatmap()` object visualization of cluster
#'  assignments.
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

#' Plot of feature values in a data list
#'
#' This plot, built on `ComplexHeatmap::Heatmap()`, visualizes the feature
#' values in a data list as a continuous heatmap with observations along the
#' columns and features along the rows.
#'
#' @param x A `data_list` object.
#' @param y Optional argument to `plot`, not used in this method.
#' @param cluster_rows Logical indicating whether to cluster the rows
#'  (observations).
#' @param cluster_columns Logical indicating whether to cluster the columns
#'  (features).
#' @param heatmap_legend_param A list of parameters for the heatmap legend.
#' @param row_title Title for the rows (observations).
#' @param column_title Title for the columns (features).
#' @param show_row_names Logical indicating whether to show row names.
#' @param ... Additional arguments passed to `ComplexHeatmap::Heatmap()`.
#' @return A heatmap visualization of feature values.
#' @export
plot.data_list <- function(x,
                           y = NULL,
                           cluster_rows = TRUE,
                           cluster_columns = TRUE,
                           heatmap_legend_param = NULL,
                           row_title = "Observation",
                           column_title = "Feature",
                           show_row_names = FALSE,
                           ...) {
    dl_df <- x |>
        as.data.frame() |>
        dplyr::select(-"uid")
    model_mat <- stats::model.matrix(~ . - 1, data = dl_df) |>
        scale()
    if (is.null(heatmap_legend_param)) {
        heatmap_legend_param <- list(
            title = "Scaled\nValue"
        )
    }
    ComplexHeatmap::Heatmap(
        model_mat,
        cluster_rows = cluster_rows,
        cluster_columns = cluster_columns,
        #col = colours,
        heatmap_legend_param = heatmap_legend_param,
        row_title = row_title,
        column_title = column_title,
        show_row_names = show_row_names,
        ...
    )
}
