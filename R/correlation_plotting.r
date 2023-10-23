#' Display SNF cluster output in Heatmap
#'
#' Normalize SNF matrix and plot heatmap.
#'
#' @param W similarity matrix outputted from SNFtool's SNF function, with rownames and colnames in sample IDs
#' @param group_cluster cluster assignment from spectral clustering
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
#' @param cco short for Correlation of Clusters vs Outcomes (cco). It is a dataframe with columns:
#'  datatype: data type being integrated and clustered. Think of this as predictor
#'  outcomes: outcome variables computed against datatype. Think of this as outcome
#'  p_value: p_value from statistical testing on datatype clusters vs outcome
#'  size:
#' @param levels optional argument to re-arrange outcome display on x-axis
#'
#' @return plot A Manhattan plot
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
#' @param corr matrix of outcomes-outcomes correlation p_values
#' @param labels_color optional argument to specify color labels for datatypes
#' @param row_km kmean partitioning of features along rows for display
#' @param column_km kmean partitioning of features along columns for display
#'
#' @return hm a correlation heatmap
#'
#' @export
corrHeatmap <- function(corr,
                        row_km,
                        column_km,
                        labels_color = NULL) {
    # Calculate the log-10 p-value of the correlation coefficient significance
    corr_log <- log10(corr + 1)
    # Color bars
    outcome_heatmap_color <- circlize::colorRamp2(
        c(0, 0.0005, 0.005, 0.05, 1),
        c("navy", "blue", "royalblue", "steelblue2", "white")
    )
    # Add color for row and column labels
    if (is.null(labels_color)) {
        labels_color <- c(rep("black", ncol(corr_log)))
        names(labels_color) <- colnames(corr_log)
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
        column_names_gp = grid::gpar(fontsize = 9, col = labels_color),
        row_names_gp = grid::gpar(fontsize = 9, col = labels_color),
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

#' Spectral clustering from c2 to user-defined max-number of cluster. Used by plotClustersAlluvial_wOutcome function
#'
#' Do spectral clustering through cluster 2 to user defined cluster number and outputs clustering result
#'
#' @param W a similarity matrix
#' @param numc max number of cluster to do spectral clustering
#'
#' @return result_df dataframe of cluster assignments
#'
#' @export
output_cluster_file <- function(W, numc = 8){
  id_keep=rownames(W)
  # result has numc-1 cluster columns and sample names in rownames
  result_df = data.frame(matrix(NA, ncol = numc-1, nrow = nrow(W)))
  rownames(result_df) = id_keep
  for (C in 2:numc) {
    clustering = SNFtool::spectralClustering(W, C)
    result_df[,C-1] = clustering
  }
  colnames(result_df) <- c(2:numc)
  return(result_df)

}

#' Prepare dataframe for multi-cluster alluvial plot
#'
#' Calculates the frequency of different data combinations and prepares dataframe. To be used by plotClustersAlluvial_wDiagnosis function
#'
#' @param cluster_env_df is a dataframe with clustering assignments and environmental variables
#' @param fill_alluvium_by variable to color alluvium. This is an outcome variable in cluster_env_df
#' @param numc user-defined max number of clusters that went through spectral clustering
#' @param key_outcome optional key outcome such as "Disease Diagnosis" that is the ultimate outcome. To be displayed on the last x-axis alluvia
#'
#' @return df wide format dataframe
#'
#' @export
prepare_for_alluvial_wDiagnosis <- function(cluster_env_df,
                                            fill_alluvium_by,
                                            numc,
                                            key_outcome=NULL){
  if (!(is.null(key_outcome))){
    key_outcome = key_outcome
  }
  # prepare data
  # calculate frequency of each combination/row
  colnames(cluster_env_df)[1:numc-1] = paste0("c", as.character(c(2:numc)))
  outcome = fill_alluvium_by
  cluster_env_df = dplyr::select(cluster_env_df, c(1:numc-1, outcome, key_outcome))
  cluster_env_df$Fill = cluster_env_df[,fill_alluvium_by]
  # use concat to find frequency
  cluster_env_df$'concat' = apply(
      cluster_env_df,
      1,
      function(row) paste0(row, collapse="")
    )
  table = as.data.frame(table(cluster_env_df$'concat'))
  df = unique(cluster_env_df)
  df = dplyr::left_join(df, table, by = c("concat"="Var1"))
  return(df)
}

#' Plot alluvial plot
#'
#' Plot alluvial plot from 2 clusters to user-defined number of clusters along with outcome/environmental variables.
#'
#' @param W a similarity matrix with sample ids in rownames and colnames
#' @param df_env dataframe of environmental or outcome variables to color the alluvium. With "Subject_Number" column filled with sample ids
#' @param numc number of clusters
#' @param outcome column name from df_env to fill the alluvium with
#' @param key_outcome optional key outcome such as "Disease Diagnosis" that is the ultimate outcome. To be displayed as the last x-axis alluvia
#'
#'
#' @export
plotClustersAlluvial_wDiagnosis <- function(W,
                                            df_env,
                                            numc,
                                            outcome,
                                            key_outcome = NULL) {
    # Meaningless visible bindings to supply to plotting functions
    x <- ""
    Freq <- ""
    stratum <- ""
    Count <- ""
    Fill <- ""
    if (!(is.null(key_outcome))) {
        key_outcome = key_outcome
    }
    # run clustering from c2 to numc
    cluster_df = output_cluster_file(W, numc = numc)
    #merge cluster_df with variable_df
    cluster_df$Subject_Number = as.character(rownames(cluster_df))
    #merge df_env in with cluster assignment data
    merged_df = dplyr::left_join(
        cluster_df,
        df_env,
        by = "Subject_Number"
    )
    # prepare data for plotting alluvial
    alluvial_df <- prepare_for_alluvial_wDiagnosis(
        cluster_env_df = merged_df,
        fill_alluvium_by = outcome,
        numc = numc,
        key_outcome = key_outcome
    )
    if (is.null(key_outcome) & !(is.null(outcome))) { # include outcome only
      columns_to_lodes = c(1:numc)
    } else if (!(is.null(key_outcome)) & !(is.null(outcome))) { # include both outcome and key_outcome
      key_outcome = key_outcome
      columns_to_lodes = c(1:(numc+1))
    }
    # change to lode form
    alluvial_df_lodes <- ggalluvial::to_lodes_form(
        alluvial_df,
        axes = columns_to_lodes,
        id = "Count"
    ) # "x" column is column names being extended to long format;
    # "stratum" column are categories in each x axes/stratum
    # "Count" column is generated as alluvium
    # PLOT
    alluvial_df_lodes |>
        ggplot2::ggplot(
            ggplot2::aes(
                x = x,
                y = Freq,
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
        ggplot2::labs(title = outcome) +
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
}
