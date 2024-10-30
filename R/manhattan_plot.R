#' Manhattan plot of feature-feature associaiton p-values
#'
#' @param data_list List of dataframes containing data information.
#'
#' @param key_var Feature for which the association p-values of all other
#' features are plotted.
#'
#' @param neg_log_pval_thresh Threshold for negative log p-values.
#'
#' @param threshold p-value threshold to plot dashed line at.
#'
#' @param point_size Size of points in the plot.
#'
#' @param text_size Size of text in the plot.
#'
#' @param plot_title Title of the plot.
#'
#' @param hide_x_labels If TRUE, hides x-axis labels.
#'
#' @param bonferroni_line If TRUE, plots a dashed black line at the
#' Bonferroni-corrected equivalent of the p-value threshold.
#'
#' @return A Manhattan plot (class "gg", "ggplot") showing the association
#' p-values of features against one key feature in a data list.
#'
#' @export
var_manhattan_plot <- function(data_list,
                               key_var,
                               neg_log_pval_thresh = 5,
                               threshold = NULL,
                               point_size = 5,
                               text_size = 20,
                               plot_title = NULL,
                               hide_x_labels = FALSE,
                               bonferroni_line = FALSE) {
    pval_matrix <- calc_assoc_pval_matrix(data_list)
    ###########################################################################
    # Suppress warnings related to non-standard evaluation
    ###########################################################################
    variable <- ""
    neg_log_pval <- ""
    domain <- ""
    ###########################################################################
    pval_df <- data.frame(pval_matrix[, key_var, drop = FALSE])
    pval_df$"variable" <- rownames(pval_df)
    rownames(pval_df) <- NULL
    pval_df <- pval_df[pval_df$"variable" != key_var, ]
    colnames(pval_df) <- c("neg_log_pval", "variable")
    pval_df$"neg_log_pval" <- -log10(pval_df$"neg_log_pval")
    pval_df <- dplyr::mutate(
        pval_df,
        neg_log_pval = dplyr::case_when(
            neg_log_pval > neg_log_pval_thresh ~ neg_log_pval_thresh,
            neg_log_pval <= neg_log_pval_thresh ~ neg_log_pval,
            TRUE ~ NA
        )
    )
    summary_data <- dplyr::inner_join(
        pval_df,
        dl_variable_summary(data_list),
        by = dplyr::join_by("variable" == "name")
    )
    summary_data <- dplyr::arrange(summary_data, domain)
    summary_data$"variable" <- factor(
        summary_data$"variable",
        levels = unique(summary_data$"variable")
    )
    ###########################################################################
    # Base plot creation
    ###########################################################################
    plot <- summary_data |>
        ggplot2::ggplot() +
        ggplot2::geom_jitter(
            mapping = ggplot2::aes(
                group = domain,
                x = variable,
                y = neg_log_pval,
                colour = domain
            ),
            height = 0,
            width = 0,
            size = point_size
        ) +
        ggplot2::labs(
            x = NULL,
            y = expression("-log"[10] * "(p)"),
            colour = "Domain",
            title = plot_title
        ) +
        ggplot2::ylim(0, neg_log_pval_thresh) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = 90,
                vjust = 0.5,
                hjust = 1
            ),
            plot.title = ggplot2::element_text(hjust = 0.5),
            text = ggplot2::element_text(size = text_size)
        )
    if (!is.null(threshold)) {
        plot <- plot + ggplot2::geom_hline(
            yintercept = -log10(threshold),
            linetype = "dashed",
            colour = "red"
        )
    }
    if (bonferroni_line) {
        plot <- plot + ggplot2::geom_hline(
            yintercept = -log10(threshold / nrow(pval_df)),
            linetype = "dashed",
            colour = "black"
        )
    }
    ############################################################################
    ## Hide x-axis ticks
    ############################################################################
    if (hide_x_labels) {
        plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_blank())
    }
    return(plot)
}

#' Manhattan plot of feature-meta cluster associaiton p-values
#'
#' Given a dataframe of representative meta cluster solutions (see
#' `get_representative_solutions()`, returns a Manhattan plot for showing
#' feature separation across all features in provided data/target_lists.
#'
#' @param extended_solutions_matrix A solutions_matrix that contains "_pval"
#' columns containing the values to be plotted. This object is the output of
#' `extend_solutions()`.
#'
#' @param data_list List of dataframes containing data information.
#'
#' @param target_list List of dataframes containing target information.
#'
#' @param variable_order Order of features to be displayed in the plot.
#'
#' @param neg_log_pval_thresh Threshold for negative log p-values.
#'
#' @param threshold p-value threshold to plot horizontal dashed line at.
#'
#' @param point_size Size of points in the plot.
#'
#' @param text_size Size of text in the plot.
#'
#' @param plot_title Title of the plot.
#'
#' @param xints Either "outcomes" or a vector of numeric values to plot
#' vertical lines at.
#'
#' @param hide_x_labels If TRUE, hides x-axis labels.
#'
#' @param domain_colours Named vector of colours for domains.
#'
#' @return A Manhattan plot (class "gg", "ggplot") showing the association
#' p-values of features against each solution in the provided solutions matrix,
#' stratified by meta cluster label.
#'
#' @export
mc_manhattan_plot <- function(extended_solutions_matrix,
                              data_list = NULL,
                              target_list = NULL,
                              variable_order = NULL,
                              neg_log_pval_thresh = 5,
                              threshold = NULL,
                              point_size = 5,
                              text_size = 20,
                              plot_title = NULL,
                              xints = NULL,
                              hide_x_labels = FALSE,
                              domain_colours = NULL) {
    ###########################################################################
    # Ensure one of data_list or target_list is not NULL
    ###########################################################################
    if (is.null(data_list) && is.null(target_list)) {
        stop("At least one of data_list or target_list must be provided.")
    }
    ###########################################################################
    # Suppress warnings related to non-standard evaluation
    ###########################################################################
    variable <- ""
    domain <- ""
    neg_log_pval <- ""
    ###########################################################################
    # Formatting extended_solutions_matrix as dataframe
    ###########################################################################
    extended_solutions_matrix <- data.frame(extended_solutions_matrix)
    ###########################################################################
    # Select row_id, label, and p-value related columns only
    ###########################################################################
    if (!"label" %in% colnames(extended_solutions_matrix)) {
        extended_solutions_matrix$"label" <- extended_solutions_matrix$"row_id"
    }
    extended_solutions_matrix <- extended_solutions_matrix |>
        dplyr::select(
            "row_id",
            "label",
            dplyr::ends_with("_pval")
        )
    if (ncol(extended_solutions_matrix) == 2) {
        stop(
            "extended_solutions_matrix is missing p-value columns. Did you",
            " provide an unextended solutions matrix instead?"
        )
    }
    extended_solutions_matrix <- dplyr::select(
        extended_solutions_matrix,
        -dplyr::contains(c("min_pval", "mean_pval", "max_pval"))
    )
    ###########################################################################
    # Convert row_id and label to factors
    ###########################################################################
    extended_solutions_matrix$"row_id" <- factor(
        extended_solutions_matrix$"row_id"
    )
    extended_solutions_matrix$"label" <- factor(
        extended_solutions_matrix$"label"
    )
    ###########################################################################
    # Re-assign names to the data list and target list
    ###########################################################################
    if (!is.null(target_list)) {
        data_list_renamed <- data_list |> lapply(
            function(x) {
                x$"domain" <- paste0("I-", x$"domain")
                return(x)
            }
        )
        target_list_renamed <- target_list |> lapply(
            function(x) {
                x$"domain" <- paste0("O-", x$"domain")
                return(x)
            }
        )
        data_list <- c(data_list_renamed, target_list_renamed)
    }
    ###########################################################################
    # Columns that end with _pval are truncated by neg_log_pval_thresh
    ###########################################################################
    var_cols_idx <- endsWith(colnames(extended_solutions_matrix), "_pval")
    var_cols <- colnames(extended_solutions_matrix)[var_cols_idx]
    cutoff_var_cols <- extended_solutions_matrix[, var_cols] |>
        apply(
            MARGIN = 2,
            FUN = function(x) {
                p <- -log10(x)
                if (length(p) == 1) {
                    if (p > neg_log_pval_thresh) {
                        p <- neg_log_pval_thresh
                    }
                } else {
                    p[p > neg_log_pval_thresh] <- neg_log_pval_thresh
                }
                return(p)
            }
        ) |>
        as.matrix()
    if (dim(cutoff_var_cols)[2] == 1) {
        cutoff_var_cols <- t(cutoff_var_cols)
    }
    extended_solutions_matrix[, var_cols] <- cutoff_var_cols
    summary_data <- extended_solutions_matrix |>
        tidyr::pivot_longer(
            !(c("row_id", "label")),
            names_to = "variable",
            values_to = "neg_log_pval"
        ) |>
        data.frame()
    summary_data$"variable" <- sub("_pval$", "", summary_data$"variable")
    ###########################################################################
    # Merge the summmary plot with domain information from the data_list
    ###########################################################################
    dl_metadata <- summarize_dl(data_list, "feature") |> dplyr::select(-"type")
    summary_data <- merge(
        summary_data,
        dl_metadata,
        by.x = "variable",
        by.y = "name",
        all.x = TRUE
    )
    summary_data <- summary_data |> dplyr::arrange(domain)
    ###########################################################################
    # Proper ordering of features through factor level assignment
    ###########################################################################
    if (is.null(variable_order)) {
        summary_data$"variable" <- factor(
            summary_data$"variable",
            levels = unique(summary_data$"variable")
        )
    } else {
        involved_vars <- unique(summary_data$"variable")
        variable_order <- variable_order[variable_order %in% involved_vars]
        keep_vars <- summary_data$"variable" %in% variable_order
        summary_data <- summary_data[keep_vars, ]
        summary_data$"variable" <- factor(
            summary_data$"variable",
            levels = variable_order
        )
    }
    ###########################################################################
    # Base plot creation
    ###########################################################################
    plot <- summary_data |>
        ggplot2::ggplot() +
        ggplot2::geom_jitter(
            mapping = ggplot2::aes(
                group = domain,
                x = variable,
                y = neg_log_pval,
                colour = domain
            ),
            height = 0,
            width = 0,
            size = point_size
        ) +
        ggplot2::labs(
            x = NULL,
            y = expression("-log"[10] * "(p)"),
            colour = "Domain",
            title = plot_title
        ) +
        ggplot2::ylim(0, neg_log_pval_thresh) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = 90,
                vjust = 0.5,
                hjust = 1
            ),
            plot.title = ggplot2::element_text(hjust = 0.5),
            text = ggplot2::element_text(size = text_size)
        ) +
        ggplot2::facet_grid(label ~ .)
    ###########################################################################
    # Assigning colours to domains
    ###########################################################################
    prefixed_domains <- unique(summary_data$"domain")
    domains <- substr(prefixed_domains, 3, nchar(prefixed_domains))
    if (is.null(domain_colours)) {
        plot <- plot + ggplot2::scale_color_brewer(
            labels = domains,
            palette = "Set1"
        )
    } else {
        domain_colour_order <- order(match(names(domain_colours), domains))
        domain_colours <- domain_colours[domain_colour_order]
        colour_labels <- names(domain_colours)
        colours <- domain_colours
        names(colours) <- NULL
        plot <- plot + ggplot2::scale_color_manual(
            labels = colour_labels,
            values = colours
        )
    }
    ###########################################################################
    # Hide x-axis ticks
    ###########################################################################
    if (hide_x_labels) {
        plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_blank())
    }
    ###########################################################################
    # Prepping x-intercept positions
    ###########################################################################
    n_vars <- length(unique(summary_data$"variable"))
    target_rows <- startsWith(summary_data$"domain", "O")
    n_outcomes <- length(unique(summary_data[target_rows, "variable"]))
    if (is.null(xints) && !is.null(data_list) && !is.null(target_list)) {
        plot <- plot + ggplot2::geom_vline(
            xintercept = n_vars - n_outcomes + 0.5
        )
    } else if (!is.null(xints)) {
        xints <- xints + 0.5
        plot <- plot + ggplot2::geom_vline(
            xintercept = xints
        )
    }
    ###########################################################################
    # Add p-value threshold if requested
    ###########################################################################
    if (!is.null(threshold)) {
        plot <- plot + ggplot2::geom_hline(
            yintercept = -log10(threshold),
            linetype = "dashed",
            colour = "red"
        )
    }
    return(plot)
}

#' Manhattan plot of feature-cluster association p-values
#'
#' @param esm Extended solutions matrix storing associations between features
#' and cluster assignments. See `?extend_solutions`.
#'
#' @param neg_log_pval_thresh Threshold for negative log p-values.
#'
#' @param threshold P-value threshold to plot dashed line at.
#'
#' @param point_size Size of points in the plot.
#'
#' @param jitter_width Width of jitter.
#'
#' @param jitter_height Height of jitter.
#'
#' @param text_size Size of text in the plot.
#'
#' @param plot_title Title of the plot.
#'
#' @param hide_x_labels If TRUE, hides x-axis labels.
#'
#' @param bonferroni_line If TRUE, plots a dashed black line at the
#'  Bonferroni-corrected equivalent of the p-value threshold.
#'
#' @return A Manhattan plot (class "gg", "ggplot") showing the association
#' p-values of features against each solution in the provided solutions matrix.
#'
#' @export
esm_manhattan_plot <- function(esm,
                               neg_log_pval_thresh = 5,
                               threshold = NULL,
                               point_size = 5,
                               jitter_width = 0.1,
                               jitter_height = 0.1,
                               text_size = 15,
                               plot_title = NULL,
                               hide_x_labels = FALSE,
                               bonferroni_line = FALSE) {
    pval_df <- get_pvals(esm, keep_summaries = FALSE)
    ###########################################################################
    # Columns that end with _pval are truncated by neg_log_pval_thresh
    ###########################################################################
    var_cols <- colnames(pval_df)[2:ncol(pval_df)]
    cutoff_var_cols <- pval_df[, var_cols] |>
        apply(
            MARGIN = 2,
            FUN = function(x) {
                p <- -log10(x)
                if (length(p) == 1) {
                    if (p > neg_log_pval_thresh) {
                        p <- neg_log_pval_thresh
                    }
                } else {
                    p[p > neg_log_pval_thresh] <- neg_log_pval_thresh
                }
                return(p)
            }
        ) |>
        as.matrix()
    if (dim(cutoff_var_cols)[2] == 1) {
        cutoff_var_cols <- t(cutoff_var_cols)
    }
    pval_df[, var_cols] <- cutoff_var_cols
    ###########################################################################
    # Suppress global visible binding errors during building
    row_id <- ""
    variable <- ""
    pval <- ""
    pval_df$"row_id" <- factor(pval_df$"row_id")
    pval_df <- pval_df |>
        tidyr::pivot_longer(
            !row_id,
            names_to = "variable",
            values_to = "pval"
        ) |>
        data.frame()
    pval_df$"variable" <- gsub("_pval", "", pval_df$"variable")
    plot <- pval_df |>
        ggplot2::ggplot() +
        ggplot2::geom_jitter(
            mapping = ggplot2::aes(
                x = variable,
                y = pval,
                colour = row_id
            ),
            width = jitter_width,
            height = jitter_height,
            alpha = 1,
            size = point_size
        ) +
        ggplot2::labs(
            x = NULL,
            y = expression("-log"[10] * "(p)"),
            colour = "Solution",
            title = plot_title
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = 90,
                vjust = 0.5,
                hjust = 1
            ),
            plot.title = ggplot2::element_text(
                hjust = 0.5
            ),
            text = ggplot2::element_text(size = text_size)
        )
    if (!is.null(threshold)) {
        plot <- plot + ggplot2::geom_hline(
            yintercept = -log10(threshold),
            linetype = "dashed",
            colour = "red"
        )
    }
    if (bonferroni_line) {
        plot <- plot + ggplot2::geom_hline(
            yintercept = -log10(threshold / nrow(pval_df)),
            linetype = "dashed",
            colour = "black"
        )
    }
    ###########################################################################
    # Hide x-axis ticks
    ###########################################################################
    if (hide_x_labels) {
        plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_blank())
    }
    return(plot)
}
