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
#' @param save_path optional path to save figure to
#'
#' @export
ak_heatmap <- function(ak_scan_om, min_or_mean = "min", save_path = NULL) {
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
    if (!(is.null(save_path))) {
        pheatmap::pheatmap(
            a_k_matrix,
            cluster_rows = FALSE,
            cluster_cols = FALSE,
            filename = save_path)
    }
    pheatmap::pheatmap(
        a_k_matrix,
        cluster_rows = FALSE,
        cluster_cols = FALSE)
}
