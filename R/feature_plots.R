#' Jitter plot separating a feature by cluster
#'
#' @param df A data.frame containing cluster column and the feature to plot.
#'
#' @param feature The feature to plot.
#'
#' @export
jitter_plot <- function(df, feature) {
    ###########################################################################
    # Suppress NSE warnings
    ###########################################################################
    cluster <- ""
    keycol <- ""
    df$"cluster" <- as.factor(df$"cluster")
    df <- df |> dplyr::rename("keycol" = !!feature)
    plot <- df |>
        dplyr::select(
            cluster,
            keycol
        ) |>
        ggplot2::ggplot(
            ggplot2::aes(
                x = cluster,
                y = keycol,
                color = cluster
            )
        ) +
        ggplot2::geom_violin(
            alpha = 0.4
        ) +
        ggplot2::geom_jitter(
            height = 0.1,
            width = 0.2,
            alpha = 0.5,
            size = 3
        ) +
        ggplot2::stat_summary(
            fun = "mean",
            geom = "point",
            colour = "black",
            size = 5
        ) +
        ggplot2::labs(
            x = "cluster",
            y = feature,
            color = "cluster"
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            text = ggplot2::element_text(size = 20),
            legend.position = "none",
            panel.grid.major.x = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(size = 20)
        )
    return(plot)
}

#' Bar plot separating a feature by cluster
#'
#' @param df A data.frame containing cluster column and the feature to plot.
#'
#' @param feature The feature to plot.
#'
#' @export
bar_plot <- function(df, feature) {
    ###########################################################################
    # Suppress NSE warnings
    ###########################################################################
    cluster <- ""
    keycol <- ""
    percent <- ""
    n <- ""
    df$"cluster" <- as.factor(df$"cluster")
    df <- df |>
        dplyr::rename("keycol" = !!feature) |>
        dplyr::select(cluster, keycol) |>
        dplyr::group_by(cluster) |>
        dplyr::count(keycol) |>
        dplyr::mutate(percent = n / sum(n) * 100)
    df$"keycol" <- factor(df$"keycol")
    if (all(df$"keycol" %in% c(0, 1))) {
        levels(df$"keycol") <- c("1", "0")
    }
    plot <- df |>
        ggplot2::ggplot(
            ggplot2::aes(
                x = cluster,
                y = percent,
                fill = keycol
            )
        ) +
        ggplot2::geom_bar(
            stat = "identity",
            position = ggplot2::position_stack()
        ) +
        ggplot2::geom_text(
            ggplot2::aes(
                label = n,
                y = percent
            ),
            size = 6,
            position = ggplot2::position_stack(vjust = 0.5)
        ) +
        ggplot2::labs(
            x = "cluster",
            y = "%",
            fill = feature
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            text = ggplot2::element_text(size = 20),
            legend.text = ggplot2::element_text(size = 20),
            legend.title = ggplot2::element_text(size = 20),
            legend.position = "right",
            panel.grid.major.x = ggplot2::element_blank()
        )
    return(plot)
}

#' Automatically plot features across clusters
#'
#' Given a single row of a solutions matrix and data provided through
#' `data_list` and/or `target_list` arguments, this function will
#' return a series of bar and/or jitter plots based on feature types.
#'
#' @param solutions_matrix_row A single row of a solutions matrix.
#'
#' @param data_list A data_list containing data to plot.
#'
#' @param target_list A target_list containing data to plot.
#'
#' @param return_plots If `TRUE`, the function will return a list of plots.
#' If FALSE, the function will instead return the full data frame used for
#' plotting.
#'
#' @param save If a string is provided, plots will be saved and this string
#' will be used to prefix plot names.
#'
#' @param jitter_width Width of jitter plots if save is specified.
#'
#' @param jitter_height Height of jitter plots if save is specified.
#'
#' @param bar_width Width of bar plots if save is specified.
#'
#' @param bar_height Height of bar plots if save is specified.
#'
#' @export
auto_plot <- function(solutions_matrix_row = NULL,
                      data_list = NULL,
                      target_list = NULL,
                      return_plots = TRUE,
                      save = NULL,
                      jitter_width = 6,
                      jitter_height = 6,
                      bar_width = 6,
                      bar_height = 6) {
    if (nrow(solutions_matrix_row) != 1) {
        stop(
            "This function only plots a single solution at a time. Please ",
            " reduce the `solutions_matrix_row` argument to a single row."
        )
    }
    ###########################################################################
    # Formatting
    ###########################################################################
    solutions_matrix_row <- data.frame(solutions_matrix_row)
    ###########################################################################
    # Generating the required cluster dataframe
    ###########################################################################
    cluster_df <- get_cluster_df(solutions_matrix_row)
    ###########################################################################
    # Generating the variable dataframe
    ###########################################################################
    if (is.null(data_list) && is.null(target_list)) {
        stop("Please provide either `data_list` or `target_list`.")
    }
    dl_df <- metasnf::collapse_dl(c(data_list, target_list))
    ###########################################################################
    # Ensure solutions_matrix and dl_df have the same subjectkey column
    ###########################################################################
    solutions_matrix_subjects <- sort(cluster_df$"subjectkey")
    dl_subjects <- sort(dl_df$"subjectkey")
    if (!identical(solutions_matrix_subjects, dl_subjects)) {
        stop(
            "The subjectkeys in the solutions_matrix and DL do not match."
        )
    }
    ###########################################################################
    # Merge cluster solution and dl_df to get full data for plotting
    ###########################################################################
    full_data <- dplyr::inner_join(cluster_df, dl_df, by = "subjectkey")
    full_data$"cluster" <- factor(full_data$"cluster")
    if (return_plots == FALSE) {
        return(full_data)
    }
    # Identifying features to plot (first cols are cluster and subjectkey)
    features <- colnames(full_data)[3:length(colnames(full_data))]
    # Generating plot for every variable
    plot_list <- list()
    for (i in seq_along(features)) {
        feature <- features[[i]]
        feature_col <- full_data[, feature]
        print(
            paste0(
                "Generating plot ",
                i, "/", length(features), ": ",
                feature
            )
        )
        if (is.numeric(feature_col) && length(unique(feature_col)) > 2) {
            plot <- jitter_plot(full_data, feature)
            h <- jitter_height
            w <- jitter_width
        } else {
            plot <- bar_plot(full_data, feature)
            h <- bar_height
            w <- bar_width
        }
        if (!is.null(save)) {
            ggplot2::ggsave(
                plot = plot,
                filename = paste0(save, "_", feature, ".png"),
                width = w,
                height = h
            )
        }
        plot_list[[i]] <- plot
        names(plot_list)[[i]] <- feature
    }
    return(plot_list)
}
