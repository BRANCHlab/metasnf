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
#' @param save optional path to save figure to
#'
#' @export
om_scatter <- function(om, save = NULL) {
    om$"nclust" <- as.factor(om$"nclust")
    min_p_val <- ""
    mean_p_val <- ""
    row_id <- ""
    nclust <- ""
    plot <- ggplot2::ggplot(om,
        ggplot2::aes(
            x = min_p_val,
            y = mean_p_val,
            label = row_id,
            color = nclust)) +
        ggplot2::geom_point() +
        ggplot2::scale_x_continuous(trans = "log10") +
        ggplot2::scale_y_continuous(trans = "log10") +
        ggplot2::geom_text(hjust = 0, vjust = 0) +
        ggplot2::xlab("Minimum CBCL log(p-value)") +
        ggplot2::ylab("Mean CBCL log(p-value)") +
        ggplot2::theme_bw() +
        ggplot2::theme(text = ggplot2::element_text(size = 20))
    if (!(is.null(save))) {
        ggplot2::ggsave(filename = save, plot = plot)
    }
    return(plot)
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


#' Bar chart CBCL values for a characterization dataframe
#'
#' @param characterization_df A merged list containing cluster, subjectkey, and
#'  various CBCL outcomes
#' @param outcome string specifying outcome of interest, e.g. `cbcl_nausea`
#' @param nclust number of clusters
#'
#' @export
cbcl_bar_chart <- function(characterization_df, outcome, nclust = NULL) {
    # A lot of trickery had to go into making this plotting function work.
    # `dplyr::count` cannot accept string parameters. To avoid this, I had to
    # rename the column of interest into the same thing every time the function
    # is run (in this case, 'keycol'). The double bangs are so that the rename
    # function can evaluate the string stored in the `outcome` variable.
    cluster <- ""
    percent <- ""
    n <- ""
    n_thresh <- ""
    n_small <- ""
    keycol <- ""
    outcome_label <- stringr::str_to_title(gsub("cbcl_", "", outcome))
    outcome_label <- gsub("_", "\n", outcome_label)
    outcome_label <- paste0(outcome_label, " ") # give some legend padding
    characterization_df <- characterization_df |>
        dplyr::rename("keycol" = !!outcome) # wizardry
    summary <- characterization_df |>
        dplyr::group_by(cluster) |>
        dplyr::count(keycol) |>
        dplyr::mutate(percent = round(n / sum(n) * 100))
    summary$"keycol" <- factor(summary$"keycol", levels = c("2", "1", "0"))
    if (is.null(nclust)) {
        summary$"cluster" <- factor(summary$"cluster")
    } else {
        summary$"cluster" <- factor(summary$"cluster",
            levels = c(as.character(1:nclust)))
    }
    summary <- summary |>
        dplyr::mutate(
            n_thresh = dplyr::case_when(
                percent < 6 ~ "",
                TRUE ~ as.character(n)),
            n_small = dplyr::case_when(
                percent < 6 ~ as.character(n),
                TRUE ~ ""))
    plot <- ggplot2::ggplot(
        data = summary,
        ggplot2::aes(
            x = cluster,
            y = percent,
            fill = keycol
            )) +
        ggplot2::geom_bar(
            stat = "identity",
            position = ggplot2::position_stack(),
            width = 0.8) +
        ggplot2::geom_text(
            mapping = ggplot2::aes(
                #label = paste0(percent, "%\n(", n, ")"),
                label = n_thresh,
                #label = n,
                y = percent),
            position = ggplot2::position_stack(
                vjust = 0.5),
            size = 10) +
        ggplot2::geom_text(
            mapping = ggplot2::aes(
                #label = paste0(percent, "%\n(", n, ")"),
                label = n_small,
                #label = n,
                y = percent),
            position = ggplot2::position_stack(
                vjust = 0.5),
            size = 4) +
        ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0, .1))) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::labs(
            x = "Cluster",
            y = "Percentage",
            fill = outcome_label) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            text = ggplot2::element_text(
                size = 30))
    return(plot)
}


#' Plot clusters across all CBCL measures
#'
#' Arrange and optionally save a grid of bar charts with cluster on the x-axis
#'  and each CBCL measure on the y-axis.
#'
#' @param om An output matrix-like structure
#' @param cbcl_list List containing all CBCL dataframes
#' @param fig_path_fn Closure specifying the location to save the final figure
#' @param save_prefix prefix to add to each file saved
#' @param save_suffix suffix to add to each file saved
#' @param include a string or list of strings specifying which CBCL measures
#'  should be included. All other CBCL measures will be excluded.    
#' @param exclude a string or list of strings specifying which CBCL measures
#'  should be excluded. All other CBCL measures will be excluded.
#' @param w the width of the final saved plots
#' @param h the height of the final saved plots
#'
#' @export
plot_all_cbcl <- function(om, cbcl_list, fig_path_fn, save_prefix = NULL,
                          save_suffix = NULL, include = NULL, exclude = NULL,
                          w = 25, h = 20) {
    # Optional filtering to specified CBCL measures
    cbcl_names <- lapply(cbcl_list, function(x) { colnames(x)[2] })
    cbcl_names_short <- gsub("cbcl_", "", cbcl_names)
    print(cbcl_names_short)
    # If exclude is not null, remove any CBCL measures specified
    if (!is.null(exclude) && is.null(include)) {
        exclude <- gsub("cbcl_", "", exclude)
        cbcl_names <- cbcl_names[!cbcl_names_short %in% exclude]
    # If include is not null, keep any CBCL measures specified
    } else if (!is.null(include) && is.null(exclude)) {
        include <- gsub("cbcl_", "", include)
        cbcl_names <- cbcl_names[cbcl_names_short %in% include]
        print(cbcl_names)
    # There should be no reason to include and exclude at the same time
    } else if (!is.null(include) && !is.null(exclude)) {
        print(exclude)
        print(include)
        print(is.null(include))
        print(is.null(exclude)) 
        stop("Only one of `include` or `exclude` params should be given.")
    }
    # Iterate through the output matrix one row at a time
    for (row in seq_len(nrow(om))) {
        # Dataframe with subjectkey and assigned cluster as columns
        cluster_df <- get_cluster_df(om[row, ])
        # Where the plot for this row will be saved
        current_sig <- om[row, ]$"significance"
        save <- fig_path_fn(paste0(
            save_prefix,
            current_sig,
            save_suffix,
            ".png"), date = TRUE)
        # Making the dataframe that contains both cluster and CBCL information
        cluster_cbcl_list <- append(list(cluster_df), cbcl_list)
        characterization_df <- abcdutils::merge_df_list(cluster_cbcl_list)
        # nclust is needed for plotting functions
        nclust <- length(unique(cluster_df$"cluster"))
        # A quick print of some stats
        print(paste0("Row: ", current_sig, ". Number of clusters: ", nclust))
        print(cbcl_ord_reg(characterization_df, bonferroni = FALSE))
        # Making the bar charts and adding them to a list
        plot_list <- list()
        for (cbcl_name in cbcl_names) {
            cbcl_plot <- cbcl_bar_chart(characterization_df, cbcl_name, nclust)
            # Remove y-axis from plots
            cbcl_plot <- abcdutils::clean_plot(cbcl_plot, c("y", "x"))
            plot_list <- append(plot_list, list(cbcl_plot))
        }
        a <- lapply(plot_list, class)
        grid <- gridExtra::grid.arrange(grobs = plot_list)
        if (!is.null(fig_path_fn)) {
            ggplot2::ggsave(file = save, grid, width = w, height = h)
        }
    }
}


#' Scatter plot the NMI values for each row of an nmi_df
#'
#' @param nmi_df A dataframe an input column and any number of NMI columns
#' @param fig_path_fn A function that returns a full file path location
#'
#' @export
plot_nmis <- function(nmi_df, fig_path_fn = NULL) {
    solutions <- colnames(nmi_df)[2:length(colnames(nmi_df))]
    nmi <- ""
    solution <- ""
    input <- ""
    for (solution in solutions) {
        solution_df <- nmi_df |>
            dplyr::select("input", !!solution)
        colnames(solution_df) <- c("input", "nmi")
        solution_df <- dplyr::arrange(solution_df, dplyr::desc(nmi))
        solution_df$"input" <- factor(solution_df$"input",
                                      levels = solution_df$"input")
        solution_df$"nmi" <- as.numeric(solution_df$"nmi")
        nmi_plot <- ggplot2::ggplot(
            solution_df, ggplot2::aes(x = input, y = nmi)) +
            ggplot2::geom_point() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(
                angle = 90, vjust = 0.5, hjust = 1)) +
            ggplot2::ylab("NMI") +
            ggplot2::xlab("Input") +
            ggplot2::ggtitle(solution) +
            ggplot2::theme(text = ggplot2::element_text(size = 20))
        if (!is.null(fig_path_fn)) {
            path <-
                (fig_path_fn(paste0(solution, "_nmi_scatter.png"), date = TRUE))
            ggplot2::ggsave(file = path, nmi_plot, width = 10, height = 10)
        }
        print(nmi_plot)
    }
}
