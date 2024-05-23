#' Manhattan plot of p-values
#'
#' @param data Output from either the pval_select() function when key_mode is
#'  FALSE or from the calculate_associations() function when key_mode is TRUE
#' @param key_mode When TRUE, generates a plot of associations relative to a
#'  key variable (calculated previously through calculate_associations) rather
#'  than associations against a cluster solution
#' @param threshold P-value threshold to plot dashed line at.
#' @param bonferroni_line If TRUE, plots a dashed black line at the
#'  Bonferroni-corrected equivalent of the p-value threshold.
#'
#' @export
manhattan_plot <- function(data,
                           key_mode = FALSE,
                           threshold = NULL,
                           bonferroni_line = FALSE) {
    if (!key_mode) {
        # Suppress global visible binding errors during building
        row_id <- ""
        variable <- ""
        pval <- ""
        var_cols <- 2:ncol(data)
        data[, var_cols] <- data[, var_cols] |>
            apply(
                MARGIN = 2,
                FUN = function(x) {
                    -log10(x)
                }
            )
        data$"row_id" <- factor(data$"row_id")
        data <- data |>
            tidyr::pivot_longer(
                !row_id,
                names_to = "variable",
                values_to = "pval"
            ) |>
            data.frame()
        plot <- data |>
            ggplot2::ggplot() +
            ggplot2::geom_point(
                mapping = ggplot2::aes(
                    x = variable,
                    y = pval,
                    colour = row_id
                ),
                alpha = 1,
                size = 5
            ) +
            ggplot2::labs(
                x = "Variable",
                y = expression("-log"[10]*"(p)"),
                colour = "Solution"
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(
                axis.text.x = ggplot2::element_text(
                    angle = 90,
                    vjust = 0.5,
                    hjust = 1
                ),
                plot.title = ggplot2::element_text(hjust = 0.5)
            )
    } else {
        # Suppress global visible binding errors during building
        name <- ""
        pval <- ""
        domain <- ""
        data$"pval" <- -log10(data$"pval")
        plot <- data |>
            ggplot2::ggplot(
                mapping = ggplot2::aes(
                    x = name,
                    y = pval,
                    colour = domain,
                    group = domain
                )
            ) +
            ggplot2::geom_point(
                alpha = 1,
                size = 5
            ) +
            ggplot2::labs(
                x = "Variable",
                y = expression("-log"[10]*"(p)"),
                colour = "Domain"
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(
                axis.text.x = ggplot2::element_text(
                    angle = 90,
                    vjust = 0.5,
                    hjust = 1
                ),
                plot.title = ggplot2::element_text(hjust = 0.5)
            )
    }
    if (!is.null(threshold)) {
        plot <- plot + ggplot2::geom_hline(
            yintercept = -log10(threshold),
            linetype = "dashed",
            colour = "red"
        )
        if (bonferroni_line) {
            plot <- plot + ggplot2::geom_hline(
                yintercept = -log10(threshold / nrow(data)),
                linetype = "dashed",
                colour = "black"
            )
        }
    } else if (bonferroni_line) {
        stop(
            "Please specify threshold p-value which will be used",
            " for calculting the Bonferroni-corrected line."
        )
    }
    return(plot)
}

#' Manhattan plots of variable separation across meta clusters
#'
#' Given a dataframe of representative meta cluster solutions (see
#' `get_representative_solutions()`, returns a Manhattan plot for showing
#' variable separation across all variables in provided data/target_lists.
#'
#' @param rep_solution The dataframe of representative solutions from the
#' `get_representative_solutions()` function.
#'
#' @param threshold p-value threshold to plot horizontal dashed line at.
#'
#' @param data_list List of dataframes containing data information.
#'
#' @param target_list List of dataframes containing target information.
#'
#' @param variable_order Order of variables to be displayed in the plot.
#'
#' @param xints Either "outcomes" or a vector of numeric values to plot
#' vertical lines at.
#'
#' @param text_size Size of text in the plot.
#'
#' @param point_size Size of points in the plot.
#'
#' @param neg_log_p_thresh Threshold for negative log p-values.
#'
#' @param hide_x_labels If TRUE, hides x-axis labels.
#'
#' @param domain_colours Named vector of colours for domains.
#'
#' @export
mc_manhattan_plot <- function(rep_solution,
                              threshold = NULL,
                              data_list,
                              target_list,
                              variable_order = NULL,
                              xints = "outcomes",
                              text_size = 20,
                              point_size = 5,
                              neg_log_p_thresh = 5,
                              hide_x_labels = FALSE,
                              domain_colours = NULL) {
    ###########################################################################
    # Suppress warnings related to non-standard evaluation
    ###########################################################################
    variable <- ""
    domain <- ""
    neg_log_pval <- ""
    ###########################################################################
    # Formatting rep_solution as dataframe
    ###########################################################################
    rep_solution <- data.frame(rep_solution)
    ###########################################################################
    # Select row_id, label, and p-value related columns only
    ###########################################################################
    if (!"label" %in% colnames(rep_solution)) {
        rep_solution$"label" <- rep_solution$"row_id"
    }
    rep_solution <- rep_solution |>
        dplyr::select(
            "row_id",
            "label",
            dplyr::ends_with("_pval")
        )
    rep_solution <- dplyr::select(
        rep_solution,
        -dplyr::contains(c("min_pval", "mean_pval", "max_pval"))
    )
    ###########################################################################
    # Convert row_id and label to factors
    ###########################################################################
    rep_solution$"row_id" <- factor(rep_solution$"row_id")
    rep_solution$"label" <- factor(rep_solution$"label")
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
    # Columns that end with _p are truncated by neg_log_p_thresh
    ###########################################################################
    var_cols_idx <- endsWith(colnames(rep_solution), "_pval")
    var_cols <- colnames(rep_solution)[var_cols_idx]
    cutoff_var_cols <- rep_solution[, var_cols] |>
        apply(
            MARGIN = 2,
            FUN = function(x) {
                p <- -log10(x)
                if (length(p) == 1) {
                    if (p > neg_log_p_thresh) {
                        p <- neg_log_p_thresh
                    }
                } else {
                    p[p > neg_log_p_thresh] <- neg_log_p_thresh
                }
                return(p)
            }
        ) |>
        as.matrix()
    if (dim(cutoff_var_cols)[2] == 1) {
        cutoff_var_cols <- t(cutoff_var_cols)
    }
    rep_solution[, var_cols] <- cutoff_var_cols
    summary_data <- rep_solution |>
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
    # Proper ordering of variables through factor level assignment
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
            colour = "Domain"
        ) +
        ggplot2::ylim(0, neg_log_p_thresh) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text( # nolint
                angle = 90,
                vjust = 0.5,
                hjust = 1
            ),
            plot.title = ggplot2::element_text(hjust = 0.5),
            text = element_text(size = text_size)
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
    if (!is.null(xints)) {
        if (identical(xints, "outcomes") && n_outcomes > 0) {
            plot <- plot + ggplot2::geom_vline(
                xintercept = n_vars - n_outcomes + 0.5
            )
        } else if (is.numeric(xints)) {
            xints <- xints + 0.5
            plot <- plot + ggplot2::geom_vline(
                xintercept = xints
            )
        }
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
