#' (DEPRECATED) Function for label propagating results in a solutions_matrix
#'
#' @param solutions_matrix A solutions_matrix (training set only)
#' @param full_data_list A data_list containing training and testing subjects
#' @param clust_algs_list The clustering algorithms list used to create the
#' original solutions matrix (if any was used)
#' @param distance_metrics_list The distance metrics list used to create the
#' original solutions matrix (if any was used)
#' @param weights_matrix The weights matrix used to create the original
#' solutions matrix (if any was used)
#'
#' @export
lp_row <- function(solutions_matrix,
                   full_data_list,
                   clust_algs_list = NULL,
                   distance_metrics_list = NULL,
                   weights_matrix = NULL) {
    stop("This function is deprecated. Use lp_solutions_matrix instead.")
}

#' (DEPRECATED) Select p-values from solutions matrix
#' Replaced with "pval_select'
#'
#' @param solutions_matrix The output of batch_snf
#'
#' @return p_val_matrix P-values ready for heatmap plotting
#'
#' @export
p_val_select <- function(solutions_matrix) {
    warning("This function has been replaced by `pval_select`.")
    return(pval_select(solutions_matrix))
}

#' (DEPRECATED) Select p-values from an extended solutions matrix
#'
#' This function can be used to neatly format the p-values associated with an
#' extended solutions matrix. It can also calculate the negative logs of those
#' p-values to make it easier to interpret large-scale differences.
#'
#' @param extended_solutions_matrix The output of `extend_solutions`. A
#' dataframe that contains at least one p-value column ending in "_p".
#' @param negative_log If TRUE, will replace p-values with negative log
#' p-values.
#'
#' @export
pval_select <- function(extended_solutions_matrix,
                        negative_log = FALSE) {
    message(
        "This function has been replaced by `get_pvals`.",
        " The updated function no longer preserves or manages pval_summaries.",
        " To obtain pvalue summaries (min/mean/max), see the `pval_summaries`",
        " function."
    )
    # Select p-value columns and convert to numeric
    pval_df <- extended_solutions_matrix |>
        dplyr::select(
            "row_id",
            dplyr::ends_with("_p"),
            dplyr::contains("p_val")
        ) |>
        data.frame() |>
        metasnf::numcol_to_numeric()
    # Convert p-values to negative log p-values if requested
    if (negative_log) {
        # Remove summary columns from non-negative log p-value calculations
        pval_df <- dplyr::select(
            pval_df,
            -dplyr::contains(c("min_p_val", "mean_p_val"))
        )
        # Negative log conversions
        neg_log_pval_df <- -log(pval_df)
        neg_log_pval_df$"row_id" <- pval_df$"row_id"
        pval_df <- neg_log_pval_df
        mini_df <- pval_df |> dplyr::select(
            dplyr::ends_with("_p")
        )
        pval_df$"mean_neglog_p" <- apply(mini_df, 1, FUN = mean)
        pval_df$"max_neglog_p" <- apply(mini_df, 1, FUN = max)
    }
    return(pval_df)
}

#' (DEPRECATED) Add minimum and mean p-values to an extended solutions matrix
#'
#' @param solutions_matrix A solutions_matrix object that already has some
#' p-value columns included.
#'
#' @param na_rm If TRUE, NA values will be removed for mean/min calculations
#'
#' @export
pval_summaries <- function(solutions_matrix, na_rm = TRUE) {
    pval_cols <- solutions_matrix |>
        dplyr::select(dplyr::ends_with("_p"))
    pval_cols <- numcol_to_numeric(pval_cols)
    if (na_rm) {
        mean_pvals <- apply(
            pval_cols,
            1,
            FUN = function(x) {
                mean(x, na.rm = TRUE)
            }
        )
        min_pvals <- apply(
            pval_cols,
            1,
            FUN = function(x) {
                min(x, na.rm = TRUE)
            }
        )
    } else {
        mean_pvals <- apply(pval_cols, 1, FUN = mean)
        min_pvals <- apply(pval_cols, 1, FUN = min)
    }
    solutions_matrix$"min_p" <- min_pvals
    solutions_matrix$"mean_p" <- mean_pvals
    return(solutions_matrix)
}

#' Functions to calculate correlation between cluster assignment to outcome
#' variables and visualize to find meaningful clusters with Manhattan plot
#'
#' Calculate correlation of clusters to each outcome using chi-squared
#' (categorical outcome) and/or kruskal-wallis test (continuous outcome) in
#' each data set that were integrated using SNF, and then generates long
#' format data input for ClustersToOutcomeManhattan
#'
#' @param df a dataframe of samples with cluster_column and outcomes columns.
#' @param outcomes one or more outcomes of interest from df
#' @param method correlation test method ("chi-squared" or "kruskal")
#' @param datatype name of the SNF integration datatype
#' @param size sample size of the datatype integrated
#'
#' @return out a dataframe of correlation test results of each cluster
#' categories vs outcomes. output can be fed directly into
#' clusterToOutcomeManhattan
#'
#' @export
clusterToOutcomeCorr <- function(df,
                                 #cluster,
                                 outcomes,
                                 method,
                                 datatype,
                                 size) {
  # create and populate output dataframe with correlation test statistics
  out <- data.frame()
    for (outcome in outcomes) {
        print(outcome)
        table <- table(as.data.frame.matrix(df[, c('cluster', outcome)]))
        if (method == "chi-squared") {
            result <- stats::chisq.test(table, simulate.p.value = TRUE)
        } else if (method == "kruskal") {
            result <- stats::kruskal.test(df[,outcome] ~ cluster, data = df)
        }
        row <- data.frame(
            outcome,
            result$p.value,
            result$statistic,
            datatype,
            size
        )
        out <- rbind(out, row)
    }
    colnames(out) <- c("outcomes", "p_value", "statistic", "datatype", "size")
    return(out)
}

#' Display cluster assignment to outcome correlation as Manhattan plot
#'
#' Manhattan plot plots the correlation of SNF clustering to specified
#' outcome, colored by data types, dot size represents sample size.
#'
#' @param target_pvals short for Correlation of Clusters vs Outcomes (cco).
#' It is a dataframe with columns:
#'  datatype: data type being integrated and clustered. Think of this as
#'  predictor.
#'  outcomes: outcome variables computed against datatype.
#'  Think of this as outcome.
#'  p_value: p_value from statistical testing on datatype clusters vs outcome
#'  size:
#' @param levels optional argument to re-arrange outcome display on x-axis
#'
#' @return plot A Manhattan plot
#'
#' @export
clusterToOutcomeManhattan <- function(target_pvals, levels = NULL) {
    # Supplying empty values to variables accessed through dlpyr functions to
    #  enable package building
    size <- ""
    datatype <- ""
    # use levels to rearrange sequence of the outcomes
    p_value <- as.numeric(target_pvals$'p_value')
    target_pvals$'log_pvalue' <- -log10(target_pvals$'p_value')
    log_pvalue <- target_pvals$'log_pvalue'
    outcomes <- target_pvals$'outcomes'
    if (is.null(levels)) {
        levels <- unique(target_pvals$outcomes)
    }
    plot <- target_pvals |>
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
            yintercept = -log10(0.05 / nlevels(factor(target_pvals$outcomes))),
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
#' @param corr matrix of outcomes-outcomes correlation p_values
#' @param labels_colour optional argument to specify color labels for datatypes
#' @param row_km kmean partitioning of features along rows for display
#' @param column_km kmean partitioning of features along columns for display
#'
#' @return hm a correlation heatmap
#'
#' @export
corrHeatmap <- function(corr,
                        row_km,
                        column_km,
                        labels_colour = NULL) {
    # Calculate the log-10 p-value of the correlation coefficient significance
    corr_log <- log10(corr + 1)
    # Color bars
    outcome_heatmap_color <- circlize::colorRamp2(
        c(0, 0.0005, 0.005, 0.05, 1),
        c("navy", "blue", "royalblue", "steelblue2", "white")
    )
    # Add color for row and column labels
    if (is.null(labels_colour)) {
        labels_colour <- c(rep("black", ncol(corr_log)))
        names(labels_colour) <- colnames(corr_log)
    }
    hm <- ComplexHeatmap::Heatmap(
        as.matrix(corr_log),
        name = "Outcomes and Descriptors",
        cluster_rows = TRUE,
        cluster_columns = TRUE,
        # add significance notations to correlation heatmap
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
        column_names_gp = grid::gpar(fontsize = 9, col = labels_colour),
        row_names_gp = grid::gpar(fontsize = 9, col = labels_colour),
        row_km = row_km,
        column_km = row_km,
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
#' Generate legend for correlation heatmap.
#'
#' @param legend_name graph path to be saved to
#' @param labels argument to specify outcome label names
#' @param labels_color argument to specify outcome label name colors
#'
#' @return pd a collection of heatmap legends saved to a file in current path
#'
#' @export
corrHeatmap_legend <- function(legend_name,
                               labels,
                               labels_color) {
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
        nrow = length(labels),
        ncol = 2
    ) |>
        data.frame()
    colnames(outcome_label_color_scheme) <- c("Outcomes", "Color")
    outcome_label_color_scheme$Outcomes <- labels
    outcome_label_color_scheme$Color <- labels_color
    # Create a legend
    outcome_name_legend <- ComplexHeatmap::Legend(
        labels = labels,
        legend_gp = grid::gpar(fill = labels_color),
        title = "Outcomes and Descriptors",
        labels_gp = grid::gpar(fontsize = 12, col = labels_color),
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
    # collect legends in Pack legenD (pd) list
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
#' Manhattan plot showing predictor correlations to an outcome
#'
#' @param df_stat is a dataframe comprised of predictive features vs a single
#' outcome correlation test statistics. It comprises of features in rownames,
#' and columns with the following colnames:
#'  "p.value": from correlation test,
#'  "n": number of samples,
#'  "Group": datatype name,
#'  "Group_index": sequence of datatypes to be displayed
#' @param outcome name the correlations were computed against. To be
#' displayed in plot title
#' @param dataset_label labels that feed into SNF. Will be displayed along
#' x-axis of Manhattan plot
#'
#' @return plot a Manhattan plot
#'
#' @export
CorrManhattan <- function(df_stat, outcome, dataset_label) {
    # Supplying empty values to variables accessed through dlpyr functions to
    #  enable package building
    Group_Index <- ""
    BPcum <- ""
    chr_len <- ""
    p.value <- ""
    tot <- ""
    # Prepare data for manhattan plot
    df_manhattan <- df_stat |>
        # Compute chromosome size
        dplyr::group_by(Group_Index) |>
        dplyr::summarise(chr_len = 1) |>
        # Calculate cumulative position of each chromosome
        dplyr::mutate(tot = cumsum(chr_len) - chr_len) |>
        dplyr::select(-chr_len)
        # Add this info to the initial dataset
    df_manhattan <- dplyr::left_join(
        df_stat,
        df_manhattan,
        by = "Group_Index"
    ) |>
        # Add a cumulative position of each SNP
        dplyr::arrange(Group_Index, p.value) |>
        dplyr::mutate(BPcum = p.value + tot)
    # Define the x-axis
    df_axis <- df_manhattan |>
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
        ggplot2::scale_color_manual(
            values = rep(c("black", "orange"), 22)
        ) +
        # custom X axis:
        ggplot2::scale_x_continuous(
            label = dataset_label, breaks = df_axis$center
        ) +
        # Add a line at p = 0.05
        ggplot2::geom_hline(
            yintercept = -log10(0.05), linetype = "dashed", color = "red"
        ) +
        # Add the plot title
        ggplot2::ggtitle(
            label = paste("Correlation p-value of Predictors versus ", outcome)
        ) +
        ggplot2::ylab(
            label = expression(paste(-log[10], "(p.value)"))
        ) +
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
            plot.title = ggplot2::element_text(hjust = 0.5, size = 15)
        )
    return(plot)
}
