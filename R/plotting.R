#' Heatmap settings matrix based on meta-clustering results
#'
#' Normalizes the settings matrix and plots as a heatmap. Rows are reordered to
#'  match the row-clustering present within a provided meta-clustering result.
#'
#' @param settings_matrix matrix indicating parameters to iterate SNF through
#' @param order numeric vector indicating row ordering of settings matrix
#' @param show_rownames If TRUE (default), rownames are shown on heatmap
#' @param save optional path to save figure to
#' @param hide_ids boolean indicating if row_id numbers should be hidden
#'
#' @export
settings_matrix_heatmap <- function(settings_matrix,
                       order = NULL,
                       show_rownames = TRUE,
                       save = NULL,
                       hide_ids = FALSE) {
    if (!is.null(order)) {
        settings_matrix <- settings_matrix[order, ]
    }
    # Scaling everything to have a max of 1
    col_maxes <- apply(settings_matrix, 2, function(x) 1/max(x))
    scaled_matrix <- as.matrix(settings_matrix) %*% diag(col_maxes)
    colnames(scaled_matrix) <- colnames(settings_matrix)
    rownames(scaled_matrix) <- rownames(settings_matrix)
    gaps <- c(
        which(colnames(scaled_matrix) == "row_id"),
        which(colnames(scaled_matrix) == "t"),
        which(colnames(scaled_matrix) == "snf_scheme"),
        which(colnames(scaled_matrix) == "clust_alg"),
        which(colnames(scaled_matrix) == "mix_dist"),
        which(colnames(scaled_matrix) == "input_wt"),
        which(colnames(scaled_matrix) == "domain_wt")
    )
    colorscheme <- grDevices::colorRampPalette(c("green", "midnightblue"))(50)
    if (hide_ids == TRUE) {
        row_labels <- ""
    } else {
        row_labels <- rownames(scaled_matrix)
    }
    if (!(is.null(grDevices::dev.list())) && !(is.null(save))) {
        grDevices::dev.off()
    }
    if (!(is.null(save))) {
        pheatmap::pheatmap(
            scaled_matrix,
            cluster_rows = FALSE,
            cluster_cols = FALSE,
            labels_row = row_labels,
            gaps_col = gaps,
            show_rownames = show_rownames,
            color = colorscheme,
            legend = FALSE,
            fontsize = 12,
            filename = save)
    }
    pheatmap::pheatmap(
        scaled_matrix,
        cluster_rows = FALSE,
        cluster_cols = FALSE,
        labels_row = row_labels,
        gaps_col = gaps,
        show_rownames = show_rownames,
        color = colorscheme,
        legend = FALSE,
        fontsize = 12)
}

#' Pheatmap a matrix of p-values
#'
#' @param pvals a matrix of p-values
#' @param order numeric vector indicating row ordering of settings matrix
#' @param cluster_cols if TRUE, pheatmap will cluster (and rearrange) columns
#' @param show_rownames If TRUE (default), rownames are shown on heatmap
#' @param save optional path to save figure to
#' @param reverse_colours boolean to invert colours
#'
#' @export
pvals_heatmap <- function(pvals,
                           order = NULL,
                           cluster_cols = TRUE,
                           show_rownames = FALSE,
                           save = NULL,
                           reverse_colours = FALSE) {
    if ("row_id" %in% colnames(pvals)) {
        rownames(pvals) <- pvals$"row_id"
        pvals <- pvals |>
            dplyr::select(-"row_id")
    }
    if (!is.null(order)) {
        pvals <- pvals[order, ]
    }
    if (reverse_colours) {
        colours <- grDevices::colorRampPalette(
            RColorBrewer::brewer.pal(n = 7, name = "RdYlBu"))(100)
    } else {
        colours <- grDevices::colorRampPalette(
            rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")))(100)
    }
    if (!(is.null(grDevices::dev.list())) && !(is.null(save))) {
        grDevices::dev.off()
    }
    if (!(is.null(save))) {
        pheatmap::pheatmap(pvals,
            legend_breaks = c(0, 0.2, 0.4, 0.6, 0.8, max(pvals)),
            main = "",
            color = colours,
            legend_labels = c("0", "0.2", "0.4", "0.6", "0.8", "p-value\n\n"),
            cluster_rows = FALSE,
            cluster_cols = cluster_cols,
            show_rownames = show_rownames,
            legend = TRUE,
            filename = save)
    }
    pheatmap::pheatmap(pvals,
        legend_breaks = c(0, 0.2, 0.4, 0.6, 0.8, max(pvals)),
        main = "",
        color = colours,
        legend_labels = c("0", "0.2", "0.4", "0.6", "0.8", "p-value\n\n"),
        cluster_rows = FALSE,
        cluster_cols = cluster_cols,
        show_rownames = show_rownames,
        legend = TRUE)
}

#' Heatmap of cluster membership across an solutions matrix
#'
#' @param om solutions matrix
#' @param save optional path to save figure to
#'
#' @export
assigned_clust_pheatmap <- function(om, save = NULL) {
    cluster_matrix <- as.matrix(subs(om)[, -1])
    if (!(is.null(grDevices::dev.list())) && !(is.null(save))) {
        grDevices::dev.off()
    }
    if (!(is.null(save))) {
        pheatmap::pheatmap(
            cluster_matrix,
            cluster_rows = FALSE,
            show_rownames = FALSE,
            show_colnames = FALSE,
            legend = FALSE,
            cellwidth = 2.2,
            filename = save)
    }
    pheatmap::pheatmap(
        cluster_matrix,
        cluster_rows = FALSE,
        show_rownames = FALSE,
        show_colnames = FALSE,
        legend = FALSE,
        cellwidth = 2.2)
}

#' Scatter plot of cluster membership across an solutions matrix
#'
#' @param om solutions matrix
#' @param save optional path to save figure to
#'
#' @export
assigned_clust_scatter <- function(om, save = NULL) {
    best_mc_subs <- t(subs(om))
    colnames(best_mc_subs) <- best_mc_subs["row_id", ]
    best_mc_subs <- best_mc_subs[2:nrow(best_mc_subs), ]
    best_mc_subs <- as.data.frame(best_mc_subs)
    best_mc_subs <- sapply(best_mc_subs, as.numeric)
    row_means <- apply(X = best_mc_subs, MARGIN = 1, FUN = mean, na.rm = TRUE)
    names(row_means) <- colnames(subs(om))[-1]
    if (!(is.null(grDevices::dev.list())) && !(is.null(save))) {
        grDevices::dev.off()
    }
    if (!(is.null(save))) {
        grDevices::png(save,
            height = 5,
            width = 5,
            units = "in",
            res = 300)
        plot(row_means,
            xlab = "Subject",
            ylab = "Mean assigned cluster")
        grDevices::dev.off()
    }
    plot(row_means,
        xlab = "Subject",
        ylab = "Mean assigned cluster")
}

#' Histogram of cluster membership across an solutions matrix
#'
#' @param om solutions matrix
#' @param save optional path to save figure to
#'
#' @export
assigned_clust_hist <- function(om, save = NULL) {
    best_mc_subs <- t(subs(om))
    colnames(best_mc_subs) <- best_mc_subs["row_id", ]
    best_mc_subs <- best_mc_subs[2:nrow(best_mc_subs), ]
    best_mc_subs <- as.data.frame(best_mc_subs)
    best_mc_subs <- sapply(best_mc_subs, as.numeric)
    row_means <- apply(X = best_mc_subs, MARGIN = 1, FUN = mean, na.rm = TRUE)
    names(row_means) <- colnames(subs(om))[-1]
    if (!(is.null(grDevices::dev.list())) && !(is.null(save))) {
        grDevices::dev.off()
    }
    if (!(is.null(save))) {
        grDevices::png(save,
            height = 5,
            width = 5,
            units = "in",
            res = 300)
        graphics::hist(row_means,
            xlab = "Mean assigned cluster",
            main = NULL)
        grDevices::dev.off()
    }
    graphics::hist(row_means,
        xlab = "Mean assigned cluster",
        main = NULL)
}

#' Clean a plot
#'
#' Given a ggplot object and a list of elements to be removed, return a cleaned
#'  version of the plot.
#'
#' @param plot A ggplot object
#' @param removables A character vector of items to be removed. Can contain "x"
#'  for x-axis label, "y" for y-axis label, and "legend" to remove the legend.
#'
#' @return plot A cleaned ggplot object
#'
#' @export
clean_plot <- function(plot, removables = c()) {
    if ("x" %in% removables) {
        plot <- plot + ggplot2::xlab("")
    }
    if ("y" %in% removables) {
        plot <- plot + ggplot2::ylab("")
    }
    if ("legend" %in% removables) {
        plot <- plot + ggplot2::theme(legend.position = "none")
    }
    return(plot)
}
