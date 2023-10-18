#' Diaplay SNF cluster output in Heatmap
#'
#' @description
#' Normalize SNF matrix and plot heatmap.
#'
#' @param W similarity matrix from SNF
#' @param group_cluster cluster assignment
#' @param top_annotation annotation to be displayed above the heatmap output
#' @param left_annotation annotation to be displayed on the left of the heatmap output
#'
#' @export
displayClustersHeatmap <- function(W,
                                   group_cluster,
                                   top_annotation = NULL,
                                   left_annotation = NULL) {
    # clean matrix
    normalize <- function(X) X / rowSums(X)
    ind = sort(as.vector(group_cluster), index.return = TRUE)
    ind = ind$ix # index after arranged by cluster
    diag(W) = stats::median(as.vector(W))
    W = normalize(W)
    W = W + t(W)
    if (is.null(top_annotation) & is.null(left_annotation)) {
        ComplexHeatmap::Heatmap(
            W[ind,ind],
            cluster_rows = FALSE,
            cluster_columns = FALSE,
            show_row_names = FALSE,
            show_column_names = FALSE,
            heatmap_legend_param = list(
                color_bar = 'continuous',
                title = "Similarity")
            )
    } else {
        ComplexHeatmap::Heatmap(
            W[ind, ind],
            top_annotation = top_annotation,
            left_annotation = left_annotation,
            cluster_rows = FALSE,
            cluster_columns = FALSE,
            show_row_names = FALSE,
            show_column_names = FALSE,
            show_heatmap_legend = TRUE,
            col = NULL,
            heatmap_legend_param = list(
                color_bar = 'continuous',
                title = "Similarity"
            )
        )
    }
}

#' Functions to calculate correlation between cluster assignment to outcome variables and visualize to find meaningful clusters with Manhattan plot
#'
#' @description
#'  Calculate correlation of clusters to each outcome using chi-squared (categorical outcome) and/or kruskal-wallis test (continuous outcome) in each data set that were integrated using SNF, and then generates long format data input for ClustersToOutcomeManhattan
#'
#' @param df a dataframe of samples with cluster_column and outcomes columns.
#' @param outcomes one or more outcomes of interest from df
#' @param method correlation test method ("chi-squared" or "kruskal")
#' @param datatype name of the SNF integration datatype
#'
#' @export
clusterToOutcomeCorr <- function(df,
                                 #cluster,
                                 outcomes,
                                 method,
                                 datatype) {
    out = data.frame()
    for (outcome in outcomes) {
        print(outcome)
        table = table(as.data.frame.matrix(df[, c('cluster', outcome)]))
        if (method=="chi-squared") {
            result = stats::chisq.test(table, simulate.p.value = TRUE)
        } else if (method == "kruskal") {
            result = stats::kruskal.test(df[,outcome] ~ cluster, data = df)
        }
        row = data.frame(outcome, result$p.value, result$statistic, datatype)
        out = rbind(out, row)
    }
    return(out)
}


#' Display cluster assignment to outcome correlation as Manhattan plot
#'
#' @description
#'
#' Manhattan plot outputs the Correlation of Clusters from spectral clustering with the Outcomes (cco), colored by data types, dot size represents sample size.
#' @param cco cco #!
#' @param levels optional argument to re-arrange outcome display on x-axis
#'
#' @export
clusterToOutcomeManhattan <- function(cco, levels = NULL) {
    # Supplying empty values to variables accessed through dlpyr functions to
    #  enable package building
    size <- ""
    datatype <- ""
    # use levels to rearrange sequence of the outcomes
    p_value <- as.numeric(cco$'p_value')
    cco$'log_pvalue' <- -log10(cco$'p_value')
    log_pvalue <- cco$'log_pvalue'
    outcomes <- cco$'outcomes'
    if (is.null(levels)) {
        levels <- unique(cco$outcomes)
    }
    plot <- cco |>
        ggplot2::ggplot(
            ggplot2::aes(
                x = factor(outcomes, levels = levels),
                y = log_pvalue,
                color = factor(datatype)
            )
        ) +
        ggplot2::geom_point(
            alpha = 1,
            ggplot2::aes(size = size)
        ) +
        ggplot2::geom_hline(
            yintercept = -log10(0.05),
            linetype = "dashed",
            color = "red"
        ) +
        ggplot2::geom_hline(
            yintercept = -log10(0.05 / nlevels(factor(cco$outcomes))),
            linetype = "dashed",
            color = "black"
        ) +
        ggplot2::labs(
            x = "Outcome",
            y = "-log10(p-value)",
            color = "Data type",
            title = "Correlation p-value of SNF clusters versus Outcomes"
        ) +
        ggplot2::ylim(c(0,5)) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = 90,
                vjust = 0.5,
                hjust = 1
            ),
            plot.title = ggplot2::element_text(hjust = 0.5)
        )
    return(plot)
}

#' Generate correlation heatmap
#'
#' @description
#' Generate correlation heatmap (need more generalization. Only tested outcomes-outcomes correlation)
#'
#' @param corr matrix of outcomes-outcomes correlation p_values
#' @param outcome_label_color optional argument to specify outcome color labels
#'
#' @export
corrHeatmap <- function(corr, outcome_label_color = NULL) {
    # Calculate the log-10 p-value of the correlation coefficient significance
    corr_log <- log10(corr + 1)
    # Color bars
    outcome_heatmap_color <- circlize::colorRamp2(
        c(0, 0.0005, 0.005, 0.05, 1),
        c("navy", "blue", "royalblue", "steelblue2", "white")
    )
    # Add color for row and column labels
    if (is.null(outcome_label_color)) {
        outcome_label_color <- c(rep("black", ncol(corr_log)))
        names(outcome_label_color) <- colnames(corr_log)
    }
    hm <- ComplexHeatmap::Heatmap(
        as.matrix(corr_log),
        name = "Outcomes and Descriptors",
        cluster_rows = TRUE,
        cluster_columns = TRUE,
        cell_fun = function(j, i, x, y, width, height, fill) {
            flag <- 0
            if (corr[i, j] < 0.0001) {
                grid::grid.text(
                    "***",
                    x,
                    y,
                    hjust = 0.5,
                    vjust = 0.5,
                    gp = grid::gpar(fontsize = 12, col = "white")
                )
                flag <- 1
            }
            if (flag == 0 & corr[i, j] < 0.001) {
                grid::grid.text(
                    "**",
                    x,
                    y,
                    hjust = 0.5,
                    vjust = 0.5,
                    gp = grid::gpar(fontsize = 12, col = "white")
                )
                flag <- 1
            }
            if (flag == 0 & corr[i, j] < 0.01) {
                grid::grid.text(
                    "*",
                    x,
                    y,
                    hjust = 0.5,
                    vjust = 0.5,
                    gp = grid::gpar(fontsize = 12, col = "white")
                )
                flag <- 1
            }
        },
        column_names_gp = grid::gpar(fontsize = 9, col = outcome_label_color),
        row_names_gp = grid::gpar(fontsize = 9, col = outcome_label_color),
        row_km = 5,
        column_km = 5,
        column_dend_height = grid::unit(2, "cm"),
        row_dend_width = grid::unit(2, "cm"),
        heatmap_legend_param = list(
            title = expression(paste(log[10], "(p-value)")),
            title_gp = grid::gpar(fontsize = 15, fontface = "bold"),
            labels_gp = grid::gpar(fontsize = 10, fontface = "bold"),
            legend_height = grid::unit(6, "cm"),
            legend_width = grid::unit(2, "cm")
        ),
        col = outcome_heatmap_color,
        show_heatmap_legend = FALSE
    )
    return(hm)
}

#' Generate correlation heatmap legend
#'
#' @description
#' Generate legend for correlation heatmap.
#'
#' @param legend_name graph path to be saved to
#' @param outcome_labels optional argument to specify outcome label names
#' @param outcome_labels_color optional argument to specify outcome label name colors
#'
#' @export
corrHeatmap_legend <- function(legend_name,
                               outcome_labels,
                               outcome_labels_color) {
    # Legend for the significant p-values
    lgd_sig_01 <- ComplexHeatmap::Legend(
        pch = "*",
        type = "points",
        labels = "< 0.01",
        labels_gp = grid::gpar(fontsize = 10)
    )
    lgd_sig_001 <- ComplexHeatmap::Legend(
        pch = "**",
        type = "points",
        labels = "< 0.001",
        labels_gp = grid::gpar(fontsize = 10)
    )
    lgd_sig_0001 <- ComplexHeatmap::Legend(
        pch = "***",
        type = "points",
        labels = "< 0.0001",
        labels_gp = grid::gpar(fontsize = 10)
    )
    # Color bars
    outcome_heatmap_color <- circlize::colorRamp2(
        c(0, 0.0005, 0.005, 0.05, 1),
        c("navy", "blue", "royalblue", "steelblue2", "white")
    )
    outcome_label_color_scheme <- matrix(
        nrow = length(outcome_labels),
        ncol = 2
    ) |>
        data.frame()
    colnames(outcome_label_color_scheme) <- c("Outcomes", "Color")
    outcome_label_color_scheme$Outcomes <- outcome_labels
    outcome_label_color_scheme$Color <- outcome_labels_color
    # Create a legend
    outcome_name_legend <- ComplexHeatmap::Legend(
        labels = outcome_labels,
        legend_gp = grid::gpar(fill = outcome_labels_color),
        title = "Outcomes and Descriptors",
        labels_gp = grid::gpar(fontsize = 12, col = outcome_labels_color),
        title_gp = grid::gpar(fontsize = 15, fontface = "bold")
    )
    outcome_heatmap_lgd <- ComplexHeatmap::Legend(
        title = expression(paste(log[10], "(p-value)")),
        col_fun = outcome_heatmap_color,
        at = c(0, 0.0005, 0.005, 0.05, 1),
        labels = c("0", "0.0005", "0.005", "0.05", "1"),
        break_dist = c(1, 1, 1, 3),
        legend_height = grid::unit(6, "cm"),
        legend_width = grid::unit(2, "cm"),
        title_gp = grid::gpar(fontsize = 15, fontface = "bold")
    )
    pd <- ComplexHeatmap::packLegend(
        list = list(
            outcome_heatmap_lgd,
            lgd_sig_01,
            lgd_sig_001,
            lgd_sig_0001,
            outcome_name_legend
        )
    )
    grDevices::png(
        legend_name,
        width = 5,
        height = 10,
        units = "in",
        res = 500,
        bg = "white"
    )
    ComplexHeatmap::draw(pd)
    grDevices::dev.off()
    return(pd)
}

#' Generate predictor-outcome correlations in Manhattan plot
#'
#' @description
#' Manhattan plot showing predictor correlations to an outcome
#'
#' @param df_export is a dataframe with features in rownames, and columns:
#'  "p.value": from correlation test,
#'  "n": number of samples,
#'  "Group": datatype name,
#'  "Group_index": sequence of datatypes to be displayed
#' @param outcome name the correlations were computed against. To be displayed in plot title
#'
#' @export
CorrManhattan <- function(df_export, outcome) {
    # Supplying empty values to variables accessed through dlpyr functions to
    #  enable package building
    Group_Index <- ""
    BPcum <- ""
    chr_len <- ""
    p.value <- ""
    tot <- ""
    # Prepare data for manhattan plot
    df_manhattan <- df_export |>
        # Compute chromosome size
        dplyr::group_by(Group_Index) |>
        dplyr::summarise(chr_len = 1) |>
        # Calculate cumulative position of each chromosome
        dplyr::mutate(tot = cumsum(chr_len) - chr_len) |>
        dplyr::select(-chr_len)
        # Add this info to the initial dataset
    df_manhattan <- dplyr::left_join(
        df_export,
        df_manhattan,
        by = "Group_Index"
    ) |>
        # Add a cumulative position of each SNP
        dplyr::arrange(Group_Index, p.value) |>
        dplyr::mutate(BPcum = p.value + tot)
    # Define the x-axis
    df_axis = df_manhattan |>
        dplyr::group_by(Group_Index) |>
        dplyr::summarize(center = (max(BPcum) + min(BPcum)) / 2)
    # Prepare the plot
    plot <- df_manhattan |>
        ggplot2::ggplot(
            ggplot2::aes(x = BPcum, y = -log10(`p.value`))
        ) +
        # Show all points
        ggplot2::geom_point(
            ggplot2::aes(color = as.factor(Group_Index)),
            alpha = 0.5,
            size = 3
        ) +
        ggplot2::scale_color_manual(values = rep(c("black", "orange"), 22 )) +
        # custom X axis:
        #scale_x_continuous( label = x_axis_ticks, breaks = df_axis$center ) +
        #scale_y_continuous(expand = c(0, 0), limits = c(0, 14) ) +     # remove space between plot area and x axis
        # Add a line at p = 0.05
        ggplot2::geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
        # Add the plot title
        ggplot2::ggtitle(
            label = paste("Correlation p-value of Predictors versus ", outcome)
        ) +
        ggplot2::ylab(label = expression(paste(-log[10], "(p.value)"))) +
        # Custom the theme:
        ggplot2::theme_bw() +
        ggplot2::theme(
            legend.position = "none",
            panel.border = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(
                angle = 90,
                vjust = 0.5,
                hjust = 1,
                size = 12
            ),
            axis.text.y = ggplot2::element_text(size = 12),
            axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_text(size = 15),
            #axis.line = element_line(linewidth = 1, colour = "black"),
            plot.title = ggplot2::element_text(hjust = 0.5, size = 20)
        )
    return(plot)
}
