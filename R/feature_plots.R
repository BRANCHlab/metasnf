#' Jitter plot separating a feature by cluster
#'
#' @param df A data.frame containing cluster column and the feature to plot.
#' @param feature The feature to plot.
#' @return A jitter+violin plot (class "gg", "ggplot") showing the
#'  distribution of a feature across clusters.
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
        pick_cols(c("cluster", "keycol")) |>
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
#' @return A bar plot (class "gg", "ggplot") showing the distribution of a
#' feature across clusters.
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
        pick_cols(c("cluster", "keycol")) |>
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
            position = ggplot2::position_stack(),
            show.legend = TRUE
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
#' Given a single row of a solutions data frame and data provided through
#' a data list, this function will return a series of bar and/or
#' jitter plots based on feature types.
#'
#' @param sol_df_row A single row of a solutions data frame.
#' @param dl A data list containing data to plot.
#' @param cluster_df Directly provide a cluster_df rather than a solutions
#'  matrix. Useful if plotting data from label propagated results.
#' @param return_plots If `TRUE`, the function will return a list of plots.
#'  If FALSE, the function will instead return the full data frame used for
#'  plotting.
#' @param save If a string is provided, plots will be saved and this string
#'  will be used to prefix plot names.
#' @param jitter_width Width of jitter plots if save is specified.
#' @param jitter_height Height of jitter plots if save is specified.
#' @param bar_width Width of bar plots if save is specified.
#' @param bar_height Height of bar plots if save is specified.
#' @param verbose If TRUE, output progress to console.
#' @return By default, returns a list of plots (class "gg", "ggplot") with
#'  one plot for every feature in the provided data list and/or target list.
#'  If `return_plots` is FALSE, will instead return a single "data.frame"
#'  object containing every provided feature for every observation in long
#'  format.
#' @export
auto_plot <- function(sol_df_row = NULL,
                      dl = NULL,
                      cluster_df = NULL,
                      return_plots = TRUE,
                      save = NULL,
                      jitter_width = 6,
                      jitter_height = 6,
                      bar_width = 6,
                      bar_height = 6,
                      verbose = FALSE) {
    null_data_count <- is.null(sol_df_row) + is.null(cluster_df)
    if (null_data_count != 1) {
        metasnf_error(
            "This function requires cluster membership information to be",
            " provided through exactly one of the `sol_df_row` or",
            " `cluster_df` arguments."
        )
    }
    ###########################################################################
    # Generating the required cluster data frame
    ###########################################################################
    if (is.null(cluster_df)) {
        sol_df_row <- sol_df_row[1, ]
        cluster_df <- t(sol_df_row)
        colnames(cluster_df)[2] <- "cluster"
    }
    ###########################################################################
    # Generating the feature data frame
    ###########################################################################
    dl_df <- as.data.frame(dl)
    ###########################################################################
    # Ensure sol_df and dl_df have the same uid column
    ###########################################################################
    sol_df_uids <- sort(cluster_df$"uid")
    dl_uids <- sort(dl_df$"uid")
    if (!identical(sol_df_uids, dl_uids)) {
        metasnf_error(
            "The UIDs in the provided solutions data frame row and data list ",
            "must match."
        )
    }
    ###########################################################################
    # Merge cluster solution and dl_df to get full data for plotting
    ###########################################################################
    full_data <- dplyr::inner_join(cluster_df, dl_df, by = "uid")
    # Second column contains the cluster column
    full_data$"cluster" <- factor(full_data[, 2])
    if (return_plots == FALSE) {
        return(full_data)
    }
    # Identifying features to plot (first cols are cluster and uid)
    features <- attributes(dl)$"features"
    # Generating plot for every feature
    plot_list <- list()
    for (i in seq_along(features)) {
        feature <- features[[i]]
        feature_col <- unlist(full_data[, feature])
        if (verbose) {
            cat(
                "Generating plot ",
                i, "/", length(features), ": ",
                feature, "\n", sep = ""
            )
        }
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
