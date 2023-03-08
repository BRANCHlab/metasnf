#' Wrapper for geom_hist
#'
#' @param df A dataframe
#' @param xval x-value for histogram passed as a string
#' @param xlabel x-axis label
#' @param size x-axis label
#' @param title plot title
#'
#' @return renamed_tbi A modified form of tbi_df with clearer column names
#'
#' @export
#'
scabcd_hist <- function(df, xval, xlabel = "", title = "", size = 20) {
    ggplot2::ggplot(df, ggplot2::aes_string(x = xval)) +
        ggplot2::geom_histogram(color = "darkblue", fill = "lightblue") +
        ggplot2::ggtitle(title) +
        ggplot2::xlab(xlabel) +
        ggplot2::ylab("Count") +
        ggplot2::theme_light(base_size = size)
}

#' Wrapper for ggsave
#'
#' Saves figures to one internal and one external location.
#' The internal path is set as "figures/abcd/"
#'
#' @param path the external path location ending in trailing slash
#' @param plotname the name of the gg figure to be saved ending in .png
#'
#' @export
#'
scabcd_ggsave <- function(path, plotname) {
    carbon_path <- paste0(path, plotname)
    repo_path <- paste0("figures/abcd/", plotname)
    print("Warning: This will overwrite files located at:")
    print(paste0("[1]: ", here::here(carbon_path)))
    print(paste0("[2]: ", here::here(repo_path)))
    proceed <- readline(prompt = "Proceed? [N/y]: ")
    if (proceed == "y") {
        ## For carbon
        ggplot2::ggsave(here::here(carbon_path),
            width = 10,
            height = 10)
        # For this repo
        ggplot2::ggsave(here::here(repo_path),
            width = 10,
            height = 10)
    }
}


#' Scatter plot alpha and k hyperparameter results by minimum and mean p-values
#'
#' @param ak_scan_om output matrix containing alpha, K, and min/mean p-values
#' @param a_or_k string specifying whether alpha or K should be visualized
#'
#' @export
ak_scan_plot <- function(ak_scan_om, a_or_k = "k") {
    ak_default <- ak_scan_om |>
        dplyr::filter(K == 20, alpha == 0.5)
    min_p_val <- ""
    mean_p_val <- ""
    nclust <- ""
    K <- ""
    alpha <- ""
    if (a_or_k == "alpha" || a_or_k == "a") {
        plot <- ggplot2::ggplot(ak_scan_om, mapping = ggplot2::aes(
                x = min_p_val,
                y = mean_p_val,
                color = nclust,
                size = alpha))
    } else if (a_or_k == "k" || a_or_k == "K") {
        plot <- ggplot2::ggplot(ak_scan_om, mapping = ggplot2::aes(
                x = min_p_val,
                y = mean_p_val,
                color = nclust,
                size = K))
    } else {
        print("Visualizing alpha or K not specified. Defaulting to K.")
        plot <- ggplot2::ggplot(ak_scan_om, mapping = ggplot2::aes(
                x = min_p_val,
                y = mean_p_val,
                color = nclust,
                size = K))
    }
    plot +
        ggplot2::geom_point(data = ak_default, colour = "pink", size = 6) +
        ggplot2::geom_point() +
        ggplot2::scale_x_continuous(trans = "log10") +
        ggplot2::xlab("Minimum p-value") +
        ggplot2::ylab("Mean p-value") +
        ggplot2::theme_bw() +
        ggplot2::theme(text = ggplot2::element_text(size = 20))
}


#' Heatmap alpha and k hyperparameter results by minimum and mean p-values
#'
#' @param ak_scan_om output matrix containing alpha, K, and min/mean p-values
#' @param min_or_mean string specifying whether min or mean p-value should be
#'  visualized
#' @param save optional path to save figure to
#'
#' @export
ak_heatmap <- function(ak_scan_om, min_or_mean = "min", save = NULL) {
    if (min_or_mean == "min") {
        min_or_mean <- "min_p_val"
    } else if (min_or_mean == "mean") {
        min_or_mean <- "mean_p_val"
    } else {
        print("Min or mean p-value not specified. Defaulting to min.")
        min_or_mean <- "min_p_val"
    }
    a_k_matrix <- matrix(nrow = length(unique(ak_scan_om$"K")),
                         ncol = length(unique(ak_scan_om$"alpha")))
    colnames(a_k_matrix) <- as.character(seq(0.3, 1, by = 0.1))
    rownames(a_k_matrix) <- as.character(seq(10, 100, by = 10))
    for (i in seq_len(nrow(ak_scan_om))) {
       row <- as.character(ak_scan_om[i, "K"])
       col <- as.character(ak_scan_om[i, "alpha"])
       pval <- as.numeric(ak_scan_om[i, min_or_mean])
       a_k_matrix[row, col] <- pval
    }
    if (!(is.null(grDevices::dev.list()))) {
        grDevices::dev.off()
    }
    if (!(is.null(save))) {
        pheatmap::pheatmap(
            a_k_matrix,
            cluster_rows = FALSE,
            cluster_cols = FALSE,
            filename = save)
    }
    pheatmap::pheatmap(
        a_k_matrix,
        cluster_rows = FALSE,
        cluster_cols = FALSE)
}


#' Heatmap meta-clustering results
#'
#' @param mc_results results from meta_cluster function
#' @param save optional path to save figure to
#'
#' @export
mc_heatmap <- function(mc_results, save = NULL) {
    colnames(mc_results) <- NULL
    rownames(mc_results) <- NULL
    if (!(is.null(grDevices::dev.list()))) {
        grDevices::dev.off()
    }
    if (!(is.null(save))) {
        pheatmap::pheatmap(mc_results,
            legend_breaks = c(0, 0.5, 1, max(mc_results)),
            main = "",
            legend_labels = c("0", "0.5", "1", "ARI\n\n"),
            legend = TRUE, border_color = FALSE,
            filename = save)
    }
    pheatmap::pheatmap(mc_results,
        legend_breaks = c(0, 0.5, 1, max(mc_results)),
        main = "",
        legend_labels = c("0", "0.5", "1", "ARI\n\n"),
        legend = TRUE, border_color = FALSE)
}


#' Heatmap design matrix based on meta-clustering results
#'
#' @description
#' Normalizes the design matrix and plots as a heatmap. Rows are reordered to
#'  match the row-clustering present within a provided meta-clustering result.
#'
#' @param design_matrix matrix indicating parameters to iterate SNF through
#' @param save optional path to save figure to
#'
#' @export
dm_heatmap <- function(design_matrix, save = NULL) {
    dm_scaled <- design_matrix
    dm_scaled$"row_id" <- dm_scaled$"row_id" / max(dm_scaled$"row_id")
    dm_scaled$"K" <- dm_scaled$"K" / max(dm_scaled$"K")
    dm_scaled$"alpha" <- dm_scaled$"alpha" / max(dm_scaled$"alpha")
    dm_scaled$"snf_scheme" <-
        dm_scaled$"snf_scheme" / max(dm_scaled$"snf_scheme")
    dm_scaled$"eigen_or_rot" <-
        dm_scaled$"eigen_or_rot" / max(dm_scaled$"eigen_or_rot")
    if (!(is.null(grDevices::dev.list())) && !(is.null(save))) {
        grDevices::dev.off()
    }
    if (!(is.null(save))) {
        pheatmap::pheatmap(
            dm_scaled,
            cluster_rows = FALSE,
            cluster_cols = FALSE,
            labels_row = "",
            legend = FALSE,
            fontsize = 12,
            filename = save)
    }
    pheatmap::pheatmap(
        dm_scaled,
        cluster_rows = FALSE,
        cluster_cols = FALSE,
        labels_row = "",
        legend = FALSE,
        fontsize = 12)
}


#' Pheatmap a matrix of p-values
#'
#' @param pvals a matrix of p-values
#' @param save optional path to save figure to
#' @param reverse_colours boolean to invert colours
#'
#' @export
pvals_pheatmap <- function(pvals, save = NULL, reverse_colours = FALSE) {
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
            legend = TRUE,
            filename = save)
    }
    pheatmap::pheatmap(pvals,
        legend_breaks = c(0, 0.2, 0.4, 0.6, 0.8, max(pvals)),
        main = "",
        color = colours,
        legend_labels = c("0", "0.2", "0.4", "0.6", "0.8", "p-value\n\n"),
        cluster_rows = FALSE,
        legend = TRUE)
}


#' Scatter plot alpha and k hyperparameter results by minimum and mean p-values
#'
#' @param om output matrix
#'
#' @export
om_scatter <- function(om) {
    min_p_val <- ""
    mean_p_val <- ""
    row_id <- ""
    ggplot2::ggplot(om,
        ggplot2::aes(x = min_p_val, y = mean_p_val, label = row_id)) +
        ggplot2::geom_point() +
        ggplot2::scale_x_continuous(trans = "log10") +
        ggplot2::scale_y_continuous(trans = "log10") +
        ggplot2::geom_text(hjust = 0, vjust = 0) +
        ggplot2::xlab("Minimum CBCL log(p-value)") +
        ggplot2::ylab("Mean CBCL log(p-value)") +
        ggplot2::theme_bw() +
        ggplot2::theme(text = ggplot2::element_text(size = 20))
}


#' Pheatmap of cluster membership across an output matrix
#'
#' @param om output matrix
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


#' Scatter plot of cluster membership across an output matrix
#'
#' @param om output matrix
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

#' Histogram of cluster membership across an output matrix
#'
#' @param om output matrix
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
