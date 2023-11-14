#' Plot heatmap of similarity matrix
#'
#' @param similarity_matrix A similarity matrix
#' @param order Vector of numbers to reorder the similarity matrix (and data
#'  if provided). Overwrites ordering specified by cluster_solution param.
#' @param cluster_solution Vector containing cluster assignments.
#' @param scale_diag Method of rescaling matrix diagonals. Can be "none"
#'  (don't change diagonals), "mean" (replace diagonals with average value of
#'  off-diagonals), or "zero" (replace diagonals with 0).
#' @param log_graph If TRUE, log transforms the graph.
#' @param cluster_rows Parameter for ComplexHeatmap::Heatmap.
#' @param cluster_columns Parameter for ComplexHeatmap::Heatmap.
#' @param show_row_names Parameter for ComplexHeatmap::Heatmap.
#' @param show_column_names Parameter for ComplexHeatmap::Heatmap.
#' @param data_list A data_list containing elements requested for annotation.
#' @param data A dataframe containing elements requested for annotation.
#' @param left_bar Named list of strings, where the strings are variables in
#'  df that should be used for a barplot annotation on the left of the plot and
#'  the names are the names that will be used to caption the plots and their
#'  legends.
#' @param left_hm Like left_bar, but with a heatmap annotation instead of a
#'  barplot annotation.
#' @param right_bar See left_bar.
#' @param top_bar See left_bar.
#' @param bottom_bar See left_bar.
#' @param right_hm See left_hm.
#' @param top_hm See left_hm.
#' @param bottom_hm See left_hm.
#' @param annotation_colours Named list of heatmap annotations and their
#'  colours.
#' @param ... Additional parameters passed into ComplexHeatmap::Heatmap.
#'
#' @export
similarity_matrix_heatmap <- function(similarity_matrix,
                                      order = NULL,
                                      cluster_solution = NULL,
                                      scale_diag = "mean",
                                      log_graph = TRUE,
                                      cluster_rows = FALSE,
                                      cluster_columns = FALSE,
                                      show_row_names = FALSE,
                                      show_column_names = FALSE,
                                      data_list = NULL,
                                      data = NULL,
                                      left_bar = NULL,
                                      right_bar = NULL,
                                      top_bar = NULL,
                                      bottom_bar = NULL,
                                      left_hm = NULL,
                                      right_hm = NULL,
                                      top_hm = NULL,
                                      bottom_hm = NULL,
                                      annotation_colours = NULL,
                                      ...) {
    ###########################################################################
    # Assemble any provided data
    ###########################################################################
    if (!is.null(data_list)) {
        merged_df <- collapse_dl(data_list)
    }
    if (is.null(data)) {
        if (!is.null(data_list)) {
            # User didn't provide data, but did provide data list, so just use
            #  that.
            data <- merged_df
        }
    } else {
        if (!is.null(data_list)) {
            # User provided both the data and the data_list, so merge them
            data <- dplyr::inner_join(data, merged_df, by = "subjectkey")
        }
    }
    ###########################################################################
    # Ensure that annotations aren't being requested when data isn't given
    ###########################################################################
    annotation_requests <- list(
        left_bar,
        right_bar,
        top_bar,
        bottom_bar,
        left_hm,
        right_hm,
        top_hm,
        bottom_hm
    )
    any_null_annotations <- lapply(annotation_requests, is.null) |>
        unlist() |>
        any()
    if (!any_null_annotations) {
        if (is.null(data)) {
            stop(
                "You must provide data, either through a data_list or a",
                " dataframe passed in with the 'data' parameter to use",
                " annotations."
            )
        }
    }
    ###########################################################################
    # Sort the matrix and any other provided data
    ###########################################################################
    # The order can come from the following sources:
    #  - nowhere (reorder by similarity?)
    #  - cluster_solution (just sort by cluster_solution)
    #  - hard specified (sort the similarity_matrix by the order provided)
    if (is.null(order)) {
        # Order was not provided
        if (!is.null(cluster_solution)) {
            # Cluster solution was provided
            order <- sort(cluster_solution, index.return = TRUE)$"ix"
            message("Sorting by cluster solution.")
        } else {
            # Neither order nor cluster solution was provided
            order <- 1:nrow(similarity_matrix)
        }
    } else {
        message("Sorting by order.")
    }
    similarity_matrix <- similarity_matrix[order, order]
    data <- data[order, ]
    ###########################################################################
    # Log the graph if requested
    if (log_graph) {
        similarity_matrix <- log(similarity_matrix)
        title <- "log(Similarity)"
    } else {
        title <- "Similarity"
    }
    # Re-scale diagonals
    similarity_matrix <- scale_diagonals(
        similarity_matrix,
        method = scale_diag
    )
    # Assign breaks in the legend
    minimum <- min(similarity_matrix) |> signif(2)
    maximum <- max(similarity_matrix) |> signif(2)
    middle <- mean(c(minimum, maximum)) |> signif(2)
    # Generate annotations
    annotations_list <- generate_annotations_list(
        df = data,
        left_hm = left_hm,
        right_hm = right_hm,
        top_hm = top_hm,
        bottom_hm = bottom_hm,
        left_bar = left_bar,
        right_bar = right_bar,
        top_bar = top_bar,
        bottom_bar = bottom_bar,
        annotation_colours = annotation_colours
    )
    args_list <- list(...)
    if (is.null(args_list$"top_annotation")) {
        args_list$"top_annotation" <- annotations_list$"top_annotations"
    }
    if (is.null(args_list$"left_annotation")) {
        args_list$"left_annotation" <- annotations_list$"left_annotations"
    }
    if (is.null(args_list$"right_annotation")) {
        args_list$"right_annotation" <- annotations_list$"right_annotations"
    }
    if (is.null(args_list$"bottom_annotation")) {
        args_list$"bottom_annotation" <- annotations_list$"bottom_annotations"
    }
    # Plot
    args_list$"matrix" <- similarity_matrix
    args_list$"cluster_rows" <- cluster_rows
    args_list$"cluster_columns" <- cluster_columns
    args_list$"show_row_names" <- show_row_names
    args_list$"show_column_names" <- show_column_names
    args_list$"heatmap_legend_param" <- list(
        color_bar = "continuous",
        title = title,
        at = c(minimum, middle, maximum)
    )
    heatmap <- suppressMessages(
        do.call(
            ComplexHeatmap::Heatmap,
            args_list
        )
    )
    return(heatmap)
}

#' Functions to calculate correlation between cluster assignment to outcome variables and visualize to find meaningful clusters with Manhattan plot
#'
#' Calculate correlation of clusters to each outcome using chi-squared (categorical outcome) and/or kruskal-wallis test (continuous outcome) in each data set that were integrated using SNF, and then generates long format data input for ClustersToOutcomeManhattan
#'
#' @param df a dataframe of samples with cluster_column and outcomes columns.
#' @param outcomes one or more outcomes of interest from df
#' @param method correlation test method ("chi-squared" or "kruskal")
#' @param datatype name of the SNF integration datatype
#' @param size sample size of the datatype integrated
#'
#' @return out a dataframe of correlation test results of each cluster categories vs outcomes. output can be fed directly into clusterToOutcomeManhattan
#'
#' @export
clusterToOutcomeCorr <- function(df,
                                 #cluster,
                                 outcomes,
                                 method,
                                 datatype,
                                 size) {
  # create and populate output dataframe with correlation test statistics
  out = data.frame()
    for (outcome in outcomes) {
        print(outcome)
        table = table(as.data.frame.matrix(df[, c('cluster', outcome)]))
        if (method=="chi-squared") {
            result = stats::chisq.test(table, simulate.p.value = TRUE)
        } else if (method == "kruskal") {
            result = stats::kruskal.test(df[,outcome] ~ cluster, data = df)
        }
        row = data.frame(outcome, result$p.value, result$statistic, datatype, size)
        out = rbind(out, row)
    }
    colnames(out) = c("outcomes", "p_value", "statistic", "datatype", "size")
    return(out)
}

#' Display cluster assignment to outcome correlation as Manhattan plot
#'
#' Manhattan plot plots the correlation of SNF clustering to specified outcome, colored by data types, dot size represents sample size.
#'
#' @param target_pvals short for Correlation of Clusters vs Outcomes (cco). It is a dataframe with columns:
#'  datatype: data type being integrated and clustered. Think of this as predictor
#'  outcomes: outcome variables computed against datatype. Think of this as outcome
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
            # in grid.text, corr[i,j] specify values correlation matrix, default.units = "npc"
            # in grid.text, by default x = unit(0.5, "npc")
            # in grid.text, by default y = unit(0.5, "npc")
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
#' @param df_stat is a dataframe comprised of predictive features vs a single outcome correlation test statistics. It comprises of features in rownames, and columns with the following colnames:
#'  "p.value": from correlation test,
#'  "n": number of samples,
#'  "Group": datatype name,
#'  "Group_index": sequence of datatypes to be displayed
#' @param outcome name the correlations were computed against. To be displayed in plot title
#' @param dataset_label labels that feed into SNF. Will be displayed along x-axis of Manhattan plot
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
        ggplot2::scale_x_continuous( label = dataset_label, breaks = df_axis$center ) +
        #scale_y_continuous(expand = c(0, 0), limits = c(0, 14) ) +     # remove space between plot area and x axis
        # Add a line at p = 0.05
        ggplot2::geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
        # Add the plot title
        ggplot2::ggtitle(
            label = paste("Correlation p-value of Predictors versus ", outcome)
        ) +
        ggplot2::ylab(label = expression(paste(-log[10], "(p.value)"))) +
        #ggplot2::xlab(label = "Features") +
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
            plot.title = ggplot2::element_text(hjust = 0.5, size = 15)
        )
    return(plot)
}

#' Generate annotations list
#'
#' Intermediate function that takes in formatted lists of variables and the
#'  annotations they should be viewed through and returns annotation objects
#'  usable by ComplexHeatmap::Heatmap.
#'
#' @param df Dataframe containing all the data that is specified in the
#'  remaining arguments.
#' @param left_bar Named list of strings, where the strings are variables in
#'  df that should be used for a barplot annotation on the left of the plot and
#'  the names are the names that will be used to caption the plots and their
#'  legends.
#' @param left_hm Like left_bar, but with a heatmap annotation instead of a
#'  barplot annotation.
#' @param right_bar See left_bar.
#' @param top_bar See left_bar.
#' @param bottom_bar See left_bar.
#' @param right_hm See left_hm.
#' @param top_hm See left_hm.
#' @param bottom_hm See left_hm.
#' @param show_legend Add legends to the annotations.
#' @param annotation_colours Named list of heatmap annotations and their
#'  colours.
#'
#' @return annotations_list A named list of all the annotations.
#'
#' @export
generate_annotations_list <- function(df,
                                      left_bar = NULL,
                                      right_bar = NULL,
                                      top_bar = NULL,
                                      bottom_bar = NULL,
                                      left_hm = NULL,
                                      right_hm = NULL,
                                      top_hm = NULL,
                                      bottom_hm = NULL,
                                      show_legend = TRUE,
                                      annotation_colours = NULL) {
    ###########################################################################
    # Ensure all the variables specified are in the provided data
    check_colnames <- function(annotation_list, sorted_df) {
        if (!all(annotation_list %in% colnames(sorted_df))) {
            stop(
                "At least one variable specified for annotation is not",
                " present in the provided data_list."
            )
        }
    }
    ###########################################################################
    # Ensure every variable specified is given a name for plotting/legend
    check_listnames <- function(list) {
        if (length(list) != sum(names(list) != "", na.rm = TRUE)) {
            stop(
                "All variables provided must in the annotation lists must be",
                " named."
            )
        }
    }
    ###########################################################################
    # Initialize the possible annotations
    left_annotations <- NULL
    right_annotations <- NULL
    top_annotations <- NULL
    bottom_annotations <- NULL
    ###########################################################################
    # Top barplots
    ###########################################################################
    if (!is.null(top_bar)) {
        check_colnames(top_bar, df)
        check_listnames(top_bar)
        top_bar_names <- names(top_bar)
        #######################################################################
        # Assign names to all the variables in the top_bar
        #######################################################################
        for (i in seq_along(top_bar)) {
            ith_annotation <- ComplexHeatmap::HeatmapAnnotation(
                temporary_name = ComplexHeatmap::anno_barplot(
                    df[, top_bar[[i]]]
                ),
                show_legend = show_legend
            )
            # Remove the "temporary_name"s
            names(ith_annotation@anno_list) <- top_bar_names[[i]]
            ith_annotation@anno_list[[1]]@name <-
                top_bar_names[[i]]
            ith_annotation@anno_list[[1]]@label <-
                top_bar_names[[i]]
            ith_annotation@anno_list[[1]]@name_param$"label" <-
                top_bar_names[[i]]
            if (length(top_annotations) == 0) {
                top_annotations <- ith_annotation
            } else {
                top_annotations <- c(top_annotations, ith_annotation)
            }
        }
    }
    ###########################################################################
    # Top heatmaps
    ###########################################################################
    if (!is.null(top_hm)) {
        check_colnames(top_hm, df)
        check_listnames(top_hm)
        top_hm_names <- names(top_hm)
        #######################################################################
        # Assign names to all the variables in the top_hm
        #######################################################################
        for (i in seq_along(top_hm)) {
            if (nchar(top_hm_names[[i]]) == 0) {
                top_hm_names[[i]] <- top_hm[[i]]
            }
            ith_annotation <- ComplexHeatmap::HeatmapAnnotation(
                temporary_name = df[, top_hm[[i]]],
                show_legend = show_legend
            )
            ###################################################################
            # Check for colours
            colour_position <- which(
                names(annotation_colours) == top_hm_names[[i]]
            )
            if (sum(colour_position) != 0) {
                ith_colour_map <- annotation_colours[colour_position]
                names(ith_colour_map) <- "temporary_name"
                ith_annotation <- ComplexHeatmap::HeatmapAnnotation(
                    temporary_name = df[, top_hm[[i]]],
                    col = ith_colour_map,
                    show_legend = show_legend
                )
            } else {
                ith_annotation <- ComplexHeatmap::HeatmapAnnotation(
                    temporary_name = df[, top_hm[[i]]],
                    show_legend = show_legend
                )
            }
            ###################################################################
            # Remove the "temporary_name"s
            names(ith_annotation@anno_list) <- top_hm_names[[i]]
            ith_annotation@anno_list[[1]]@name <-
                top_hm_names[[i]]
            ith_annotation@anno_list[[1]]@label <-
                top_hm_names[[i]]
            ith_annotation@anno_list[[1]]@color_mapping@name <-
                top_hm_names[[i]]
            ith_annotation@anno_list[[1]]@name_param$"label" <-
                top_hm_names[[i]]
            if (length(top_annotations) == 0) {
                top_annotations <- ith_annotation
            } else {
                top_annotations <- c(top_annotations, ith_annotation)
            }
        }
    }
    ###########################################################################
    # Bottom barplots
    ###########################################################################
    if (!is.null(bottom_bar)) {
        check_colnames(bottom_bar, df)
        check_listnames(bottom_bar)
        bottom_bar_names <- names(bottom_bar)
        #######################################################################
        # Assign names to all the variables in the bottom_bar
        #######################################################################
        for (i in seq_along(bottom_bar)) {
            ith_annotation <- ComplexHeatmap::HeatmapAnnotation(
                temporary_name = ComplexHeatmap::anno_barplot(
                    df[, bottom_bar[[i]]]
                ),
                show_legend = show_legend
            )
            # Remove the "temporary_name"s
            names(ith_annotation@anno_list) <- bottom_bar_names[[i]]
            ith_annotation@anno_list[[1]]@name <-
                bottom_bar_names[[i]]
            ith_annotation@anno_list[[1]]@label <-
                bottom_bar_names[[i]]
            ith_annotation@anno_list[[1]]@name_param$"label" <-
                bottom_bar_names[[i]]
            if (length(bottom_annotations) == 0) {
                bottom_annotations <- ith_annotation
            } else {
                bottom_annotations <- c(bottom_annotations, ith_annotation)
            }
        }
    }
    ###########################################################################
    # Bottom heatmaps
    ###########################################################################
    if (!is.null(bottom_hm)) {
        check_colnames(bottom_hm, df)
        check_listnames(bottom_hm)
        bottom_hm_names <- names(bottom_hm)
        #######################################################################
        # Assign names to all the variables in the bottom_hm
        #######################################################################
        for (i in seq_along(bottom_hm)) {
            if (nchar(bottom_hm_names[[i]]) == 0) {
                bottom_hm_names[[i]] <- bottom_hm[[i]]
            }
            ith_annotation <- ComplexHeatmap::HeatmapAnnotation(
                temporary_name = df[, bottom_hm[[i]]],
                show_legend = show_legend
            )
            ###################################################################
            # Check for colours
            colour_position <- which(
                names(annotation_colours) == bottom_hm_names[[i]]
            )
            if (sum(colour_position) != 0) {
                ith_colour_map <- annotation_colours[colour_position]
                names(ith_colour_map) <- "temporary_name"
                ith_annotation <- ComplexHeatmap::HeatmapAnnotation(
                    temporary_name = df[, bottom_hm[[i]]],
                    col = ith_colour_map,
                    show_legend = show_legend
                )
            } else {
                ith_annotation <- ComplexHeatmap::HeatmapAnnotation(
                    temporary_name = df[, bottom_hm[[i]]],
                    show_legend = show_legend
                )
            }
            ###################################################################
            # Remove the "temporary_name"s
            names(ith_annotation@anno_list) <- bottom_hm_names[[i]]
            ith_annotation@anno_list[[1]]@name <-
                bottom_hm_names[[i]]
            ith_annotation@anno_list[[1]]@label <-
                bottom_hm_names[[i]]
            ith_annotation@anno_list[[1]]@color_mapping@name <-
                bottom_hm_names[[i]]
            ith_annotation@anno_list[[1]]@name_param$"label" <-
                bottom_hm_names[[i]]
            if (length(bottom_annotations) == 0) {
                bottom_annotations <- ith_annotation
            } else {
                bottom_annotations <- c(bottom_annotations, ith_annotation)
            }
        }
    }
    ###########################################################################
    # Left barplots
    ###########################################################################
    if (!is.null(left_bar)) {
        check_colnames(left_bar, df)
        check_listnames(left_bar)
        left_bar_names <- names(left_bar)
        #######################################################################
        # Assign names to all the variables in the left_bar
        #######################################################################
        for (i in seq_along(left_bar)) {
            ith_annotation <- ComplexHeatmap::rowAnnotation(
                temporary_name = ComplexHeatmap::anno_barplot(
                    df[, left_bar[[i]]]
                ),
                show_legend = show_legend
            )
            # Remove the "temporary_name"s
            names(ith_annotation@anno_list) <- left_bar_names[[i]]
            ith_annotation@anno_list[[1]]@name <-
                left_bar_names[[i]]
            ith_annotation@anno_list[[1]]@label <-
                left_bar_names[[i]]
            ith_annotation@anno_list[[1]]@name_param$"label" <-
                left_bar_names[[i]]
            if (length(left_annotations) == 0) {
                left_annotations <- ith_annotation
            } else {
                left_annotations <- c(left_annotations, ith_annotation)
            }
        }
    }
    ###########################################################################
    # Left heatmaps
    ###########################################################################
    if (!is.null(left_hm)) {
        check_colnames(left_hm, df)
        check_listnames(left_hm)
        left_hm_names <- names(left_hm)
        #######################################################################
        # Assign names to all the variables in the left_hm
        #######################################################################
        for (i in seq_along(left_hm)) {
            if (nchar(left_hm_names[[i]]) == 0) {
                left_hm_names[[i]] <- left_hm[[i]]
            }
            ###################################################################
            # Check for colours
            colour_position <- which(
                names(annotation_colours) == left_hm_names[[i]]
            )
            if (sum(colour_position) != 0) {
                ith_colour_map <- annotation_colours[colour_position]
                names(ith_colour_map) <- "temporary_name"
                ith_annotation <- ComplexHeatmap::rowAnnotation(
                    temporary_name = df[, left_hm[[i]]],
                    col = ith_colour_map,
                    show_legend = show_legend
                )
            } else {
                ith_annotation <- ComplexHeatmap::rowAnnotation(
                    temporary_name = df[, left_hm[[i]]],
                    show_legend = show_legend
                )
            }
            ###################################################################
            # Remove the "temporary_name"s
            names(ith_annotation@anno_list) <- left_hm_names[[i]]
            ith_annotation@anno_list[[1]]@name <-
                left_hm_names[[i]]
            ith_annotation@anno_list[[1]]@label <-
                left_hm_names[[i]]
            ith_annotation@anno_list[[1]]@color_mapping@name <-
                left_hm_names[[i]]
            ith_annotation@anno_list[[1]]@name_param$"label" <-
                left_hm_names[[i]]
            if (length(left_annotations) == 0) {
                left_annotations <- ith_annotation
            } else {
                left_annotations <- c(left_annotations, ith_annotation)
            }
        }
    }
    ###########################################################################
    # Right barplots
    ###########################################################################
    if (!is.null(right_bar)) {
        check_colnames(right_bar, df)
        check_listnames(right_bar)
        right_bar_names <- names(right_bar)
        #######################################################################
        # Assign names to all the variables in the right_bar
        #######################################################################
        for (i in seq_along(right_bar)) {
            ith_annotation <- ComplexHeatmap::rowAnnotation(
                temporary_name = ComplexHeatmap::anno_barplot(
                    df[, right_bar[[i]]]
                ),
                show_legend = show_legend
            )
            # Remove the "temporary_name"s
            names(ith_annotation@anno_list) <- right_bar_names[[i]]
            ith_annotation@anno_list[[1]]@name <-
                right_bar_names[[i]]
            ith_annotation@anno_list[[1]]@label <-
                right_bar_names[[i]]
            ith_annotation@anno_list[[1]]@name_param$"label" <-
                right_bar_names[[i]]
            if (length(right_annotations) == 0) {
                right_annotations <- ith_annotation
            } else {
                right_annotations <- c(right_annotations, ith_annotation)
            }
        }
    }
    ###########################################################################
    # Right heatmaps
    ###########################################################################
    if (!is.null(right_hm)) {
        check_colnames(right_hm, df)
        check_listnames(right_hm)
        right_hm_names <- names(right_hm)
        #######################################################################
        # Assign names to all the variables in the right_hm
        #######################################################################
        for (i in seq_along(right_hm)) {
            if (nchar(right_hm_names[[i]]) == 0) {
                right_hm_names[[i]] <- right_hm[[i]]
            }
            ith_annotation <- ComplexHeatmap::rowAnnotation(
                temporary_name = df[, right_hm[[i]]],
                show_legend = show_legend
            )
            ###################################################################
            # Check for colours
            colour_position <- which(
                names(annotation_colours) == right_hm_names[[i]]
            )
            if (sum(colour_position) != 0) {
                ith_colour_map <- annotation_colours[colour_position]
                names(ith_colour_map) <- "temporary_name"
                ith_annotation <- ComplexHeatmap::rowAnnotation(
                    temporary_name = df[, right_hm[[i]]],
                    col = ith_colour_map,
                    show_legend = show_legend
                )
            } else {
                ith_annotation <- ComplexHeatmap::rowAnnotation(
                    temporary_name = df[, right_hm[[i]]],
                    show_legend = show_legend
                )
            }
            ###################################################################
            # Remove the "temporary_name"s
            names(ith_annotation@anno_list) <- right_hm_names[[i]]
            ith_annotation@anno_list[[1]]@name <-
                right_hm_names[[i]]
            ith_annotation@anno_list[[1]]@label <-
                right_hm_names[[i]]
            ith_annotation@anno_list[[1]]@color_mapping@name <-
                right_hm_names[[i]]
            ith_annotation@anno_list[[1]]@name_param$"label" <-
                right_hm_names[[i]]
            if (length(right_annotations) == 0) {
                right_annotations <- ith_annotation
            } else {
                right_annotations <- c(right_annotations, ith_annotation)
            }
        }
    }
    ###########################################################################
    # Return final output
    annotations_list <- list(
        left_annotations = left_annotations,
        right_annotations = right_annotations,
        top_annotations = top_annotations,
        bottom_annotations = bottom_annotations
    )
    return(annotations_list)
}

#' Collapse a dataframe and/or a data_list into a single dataframe
#'
#' @param data A dataframe.
#' @param data_list A data_list.
#'
#' @export
assemble_data <- function(data, data_list) {
    if (!is.null(data_list)) {
        merged_df <- collapse_dl(data_list)
    }
    if (is.null(data)) {
        if (!is.null(data_list)) {
            # User didn't provide data, but did provide data list, so just use
            #  that.
            data <- merged_df
        }
    } else {
        if (!is.null(data_list)) {
            # User provided both the data and the data_list, so merge them
            data <- dplyr::inner_join(data, merged_df, by = "subjectkey")
        }
    }
    return(data)
}

#' Alluvial plot of patients across cluster counts and important variables
#'
#' @param cluster_sequence A list of clustering algorithms (typically, the same
#'  algorithm varied over different numbers of clusters).
#' @param similarity_matrix A similarity matrix.
#' @param data_list A data_list that contains variables to include in the plot.
#' @param data A dataframe that contains variables to include in the plot.
#' @param key_outcome The name of the variable that determines how each patient
#'  stream is coloured in the alluvial plot.
#' @param key_label Name of key outcome to be used for the plot legend.
#' @param title Title of the plot.
#' @param extra_outcomes Names of additional variables to add to the plot.
#'
#' @export
alluvial_cluster_plot <- function(cluster_sequence,
                                  similarity_matrix,
                                  data_list = NULL,
                                  data = NULL,
                                  key_outcome,
                                  key_label = key_outcome,
                                  extra_outcomes = NULL,
                                  title = NULL) {
    ###########################################################################
    # Dismissing the "no visible binding" problem during building
    ###########################################################################
    x <- ""
    Frequency <- ""
    stratum <- ""
    Count <- ""
    Fill <- ""
    ###########################################################################
    # Calculate the cluster solutions for each cluster algorithm provided
    ###########################################################################
    alluvial_df <- data.frame(subjectkey = colnames(similarity_matrix))
    for (algorithm in cluster_sequence) {
        cluster_output <- algorithm(similarity_matrix)
        solution <- cluster_output$"solution"
        nclust <- cluster_output$"nclust"
        solution_col <- data.frame(solution)
        colnames(solution_col) <- paste0("c", nclust)
        alluvial_df <- cbind(alluvial_df, solution_col)
    }
    ###########################################################################
    # Isolate variable of interest
    ###########################################################################
    data <- assemble_data(data = data, data_list = data_list)
    outcome_df_cols <- c("subjectkey", key_outcome, extra_outcomes)
    outcome_df <- data[, colnames(data) %in% outcome_df_cols]
    alluvial_df <- dplyr::inner_join(
        alluvial_df,
        outcome_df,
        by = "subjectkey"
    )
    alluvial_df <- alluvial_df |>
        dplyr::select(-dplyr::contains("subjectkey")) |>
        dplyr::group_by(dplyr::across(1:ncol(alluvial_df) - 1)) |>
        dplyr::summarize(Frequency = dplyr::n(), .groups = "keep") |>
        data.frame()
    n_alluvial_columns <- length(cluster_sequence) + 1 + length(extra_outcomes)
    alluvial_indices <- 1:n_alluvial_columns
    alluvial_df$"Fill" <- alluvial_df[, key_outcome]
    # change to lode form
    alluvial_df_lodes <- ggalluvial::to_lodes_form(
        alluvial_df,
        axes = alluvial_indices,
        id = "Count"
    )
    ###########################################################################
    # Plotting
    ###########################################################################
    plot <- alluvial_df_lodes |> ggplot2::ggplot(
        ggplot2::aes(
            x = x,
            y = Frequency,
            stratum = stratum,
            alluvium = Count
        )
    ) +
    ggalluvial::geom_alluvium(
        ggplot2::aes(fill = Fill, color = Fill)
    ) +
    ggalluvial::geom_stratum(width = 1/4) +
    ggplot2::geom_text(
        stat = ggalluvial::StatStratum,
        ggplot2::aes(
            label = ggplot2::after_stat(stratum)
        ),
        size = 3
    ) +
    ggplot2::labs(
        color = key_label,
        fill = key_label,
        title = title
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
        axis.text.x = ggplot2::element_text(
            hjust = 1,
            vjust = 0.5,
            size = 12,
            angle = 90
        ),
        axis.text.y = ggplot2::element_text(size = 12),
        axis.title.y = ggplot2::element_text(size = 15),
        axis.title.x = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 18, hjust = 0.5),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        legend.title = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 12)
    )
    return(plot)
}

#' Heatmap of pairwise associations between variables
#'
#' @param correlation_matrix Matrix containing all pairwise association
#' p-values. The recommended way to obtain this matrix is through the
#' calculate_associations function.
#' @param scale_diag Parameter that controls how the diagonals of the
#' correlation_matrix are adjusted in the heatmap. For best viewing, this is
#' set to "max", which will match the diagonals to whichever pairwise
#' association has the highest p-value.
#' @param cluster_rows Parameter for ComplexHeatmap::Heatmap. Will be ignored
#' if split_by_domain is also provided.
#' @param cluster_columns Parameter for ComplexHeatmap::Heatmap. Will be
#' ignored if split_by_domain is also provided.
#' @param show_row_names Parameter for ComplexHeatmap::Heatmap.
#' @param show_column_names Parameter for ComplexHeatmap::Heatmap.
#' @param show_heatmap_legend Parameter for ComplexHeatmap::Heatmap.
#' @param confounders A named list where the elements are columns in the
#' correlation_matrix and the names are the corresponding display names.
#' @param out_of_models Like confounders, but a named list of out of model
#' measures (who are also present as columns in the correlation_matrix).
#' @param annotation_colours Named list of heatmap annotations and their
#'  colours.
#' @param labels_colour Vector of colours to use for the columns and rows
#' of the heatmap.
#' @param split_by_domain The results of `dl_var_summar` - a dataframe that has
#' the domain of every variable in the plotted data.
#' columns of the correlation_matrix. Will be used to "slice" the heatmap into
#' visually separated sections.
#' @param ... Additional parameters passed into ComplexHeatmap::Heatmap.
#'
#' @export
correlation_pval_heatmap <- function(correlation_matrix,
                                     scale_diag = "max",
                                     cluster_rows = TRUE,
                                     cluster_columns = TRUE,
                                     show_row_names = TRUE,
                                     show_column_names = TRUE,
                                     show_heatmap_legend = FALSE,
                                     confounders = NULL,
                                     out_of_models = NULL,
                                     annotation_colours = NULL,
                                     labels_colour = NULL,
                                     split_by_domain = NULL,
                                     ...) {
    ###########################################################################
    # Format data
    ###########################################################################
    confounder_names <- confounders |> unlist() |> as.character()
    out_of_models_names <- out_of_models |> unlist() |> as.character()
    peripheral_names <- c(confounder_names, out_of_models_names)
    confounder_indices <- colnames(correlation_matrix) %in% confounder_names
    oom_indices <- colnames(correlation_matrix) %in% out_of_models_names
    peripheral_indices <- colnames(correlation_matrix) %in% peripheral_names
    confounder_data <- correlation_matrix[!peripheral_indices, confounder_indices]
    out_of_model_data <- correlation_matrix[!peripheral_indices, oom_indices]
    peripheral_data <- correlation_matrix[!peripheral_indices, peripheral_indices]
    correlation_matrix <- correlation_matrix[!peripheral_indices, !peripheral_indices]
    ###########################################################################
    if (is.null(labels_colour)) {
        labels_colour <- c(rep("black", ncol(correlation_matrix)))
        names(labels_colour) <- colnames(correlation_matrix)
    }
    title <- "p-value"
    ###########################################################################
    # Scale the matrix diagonals
    ###########################################################################
    correlation_matrix <- scale_diagonals(
        correlation_matrix,
        method = scale_diag
    )
    # Lower threshold on p-values
    correlation_matrix[correlation_matrix < 0.00001] <- 0.00001
    # Assign breaks in the legend
    minimum <- min(correlation_matrix) |> signif(2)
    maximum <- max(correlation_matrix) |> signif(2)
    middle <- mean(c(minimum, maximum)) |> signif(2)
    ###########################################################################
    # Prepare the annotations
    ###########################################################################
    in_model_colours <- c(
        "black",
        "navy",
        "blue",
        "royalblue",
        "steelblue2"
    )
    confounder_colour_values <- c(
        "black",
        "red4",
        "red2",
        "red",
        "lightcoral"
    )
    confounder_colours <- circlize::colorRamp2(
        c(0, 0.0005, 0.005, 0.05, 1),
        confounder_colour_values
    )
    out_of_model_colour_values <- c(
        "black",
        "darkgreen",
        "forestgreen",
        "chartreuse4",
        "darkseagreen1"
    )
    out_of_model_colours <- circlize::colorRamp2(
        c(0, 0.0005, 0.005, 0.05, 1),
        out_of_model_colour_values
    )
    clist <- names(confounders) |>
        lapply(
            function(x) {
                confounder_colours
            }
        )
    names(clist) <- names(confounders)
    olist <- names(out_of_models) |>
        lapply(
            function(x) {
                out_of_model_colours
            }
        )
    names(olist) <- names(out_of_models)
    annotation_colours <- c(clist, olist)
    annotations_list <- generate_annotations_list(
        df = data.frame(peripheral_data),
        left_hm = confounders,
        top_hm = out_of_models,
        annotation_colours = annotation_colours,
        show_legend = FALSE
    )
    ###########################################################################
    # Combining all the arguments to provide to ComplexHeatmap()
    ###########################################################################
    args_list <- list(...)
    if (is.null(args_list$"top_annotation")) {
        args_list$"top_annotation" <- annotations_list$"top_annotations"
    }
    if (is.null(args_list$"left_annotation")) {
        args_list$"left_annotation" <- annotations_list$"left_annotations"
    }
    args_list$"matrix" <- correlation_matrix
    args_list$"cluster_rows" <- cluster_rows
    args_list$"cluster_columns" <- cluster_columns
    args_list$"show_row_names" <- show_row_names
    args_list$"show_column_names" <- show_column_names
    args_list$"heatmap_legend_param" <- list(
        color_bar = "continuous",
        title = title,
        at = c(minimum, middle, maximum)
    )
    args_list$"col" <- circlize::colorRamp2(
        c(0, 0.0005, 0.005, 0.05, 1),
        in_model_colours
    )
    args_list$"column_names_gp" <- grid::gpar(
        fontsize = 9,
        col = labels_colour
    )
    args_list$"row_names_gp" <- grid::gpar(
        fontsize = 9,
        col = labels_colour
    )
    args_list$"show_heatmap_legend" <- show_heatmap_legend
    args_list$"cell_fun" <- cell_significance_fn(correlation_matrix)
    if (!is.null(split_by_domain)) {
        keep_vars <- split_by_domain$"name" %in% colnames(correlation_matrix)
        split_by_domain <- split_by_domain[keep_vars, ]
        args_list$"cluster_rows" <- FALSE
        args_list$"cluster_columns" <- FALSE
        args_list$"row_split" <- factor(split_by_domain$"domain")
        args_list$"column_split" <- factor(split_by_domain$"domain")
    }
    ###########################################################################
    # Create heatmap
    ###########################################################################
    heatmap <- suppressMessages(
        do.call(
            ComplexHeatmap::Heatmap,
            args_list
        )
    )
    ###########################################################################
    # Add annotation legends
    ###########################################################################
    lgd_list <- list(
        ComplexHeatmap::Legend(
            labels = c(
                "p = 1",
                "p < 0.05",
                "p < 0.005",
                "p < 0.0005",
                "p = 0"
            ),
            title = "In-Model Measures",
            type = "grid",
            legend_gp = grid::gpar(
                fill = rev(in_model_colours)
            ),
        )
    )
    if (!is.null(confounders)) {
        lgd_list[[length(lgd_list) + 1]] <- ComplexHeatmap::Legend(
            labels = c(
                "p = 1",
                "p < 0.05",
                "p < 0.005",
                "p < 0.0005",
                "p = 0"
            ),
            title = "Confounders",
            type = "grid",
            legend_gp = grid::gpar(
                fill = rev(confounder_colour_values)
            )
        )
    }
    if (!is.null(out_of_models)) {
        lgd_list[[length(lgd_list) + 1]] <- ComplexHeatmap::Legend(
            labels = c(
                "p = 1",
                "p < 0.05",
                "p < 0.005",
                "p < 0.0005",
                "p = 0"
            ),
            title = "Out-of-Model Measures",
            type = "grid",
            legend_gp = grid::gpar(
                fill = rev(out_of_model_colour_values)
            )
        )
    }
    heatmap <- ComplexHeatmap::draw(
        heatmap,
        annotation_legend_list = lgd_list
    )
}

#' Place significance stars on ComplexHeatmap cells.
#'
#' This is an internal function meant to be used to by the
#' correlation_pval_heatmap function.
#'
#' @param data The matrix containing the cells to base the significance stars
#' on.
#'
#' @return cell_fn Another function that is well-formatted for usage as the
#' cell_fun argument in ComplexHeatmap::Heatmap.
#'
#' @export
cell_significance_fn <- function(data) {
    cell_fn <- function(j, i, x, y, width, height, fill) {
        flag <- 0
        if (data[i, j] < 0.0001) {
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
        if (flag == 0 & data[i, j] < 0.001) {
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
        if (flag == 0 & data[i, j] < 0.01) {
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
    }
    return(cell_fn)
}
