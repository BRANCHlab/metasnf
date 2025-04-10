#' Manhattan plot of feature-feature association p-values
#'
#' @param dl List of data frames containing data information.
#' @param key_var Feature for which the association p-values of all other
#'  features are plotted.
#' @param neg_log_pval_thresh Threshold for negative log p-values.
#' @param threshold p-value threshold to plot dashed line at.
#' @param point_size Size of points in the plot.
#' @param text_size Size of text in the plot.
#' @param plot_title Title of the plot.
#' @param hide_x_labels If TRUE, hides x-axis labels.
#' @param bonferroni_line If TRUE, plots a dashed black line at the
#'  Bonferroni-corrected equivalent of the p-value threshold.
#' @return A Manhattan plot (class "gg", "ggplot") showing the association
#'  p-values of features against one key feature in a data list.
#' @export
#' @examples
#' dl <- data_list(
#'     list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
#'     list(income, "household_income", "demographics", "continuous"),
#'     list(pubertal, "pubertal_status", "demographics", "continuous"),
#'     list(anxiety, "anxiety", "behaviour", "ordinal"),
#'     list(depress, "depressed", "behaviour", "ordinal"),
#'     uid = "unique_id"
#' )
#' 
#' var_manhattan <- var_manhattan_plot(
#'     dl,
#'     key_var = "household_income",
#'     plot_title = "Correlation of Features with Household Income",
#'     text_size = 16,
#'     neg_log_pval_thresh = 3,
#'     threshold = 0.05
#' )
var_manhattan_plot <- function(dl,
                               key_var,
                               neg_log_pval_thresh = 5,
                               threshold = NULL,
                               point_size = 5,
                               text_size = 20,
                               plot_title = NULL,
                               hide_x_labels = FALSE,
                               bonferroni_line = FALSE) {
    pval_matrix <- calc_assoc_pval_matrix(dl)
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
        summary(dl, scope = "feature"),
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

#' Manhattan plot of feature-meta cluster association p-values
#'
#' Given a data frame of representative meta cluster solutions (see
#' `get_representative_solutions()`, returns a Manhattan plot for showing
#' feature separation across all features in provided data/target lists.
#'
#' @param ext_sol_df A sol_df that contains "_pval"
#'  columns containing the values to be plotted. This object is the output of
#'  `extend_solutions()`.
#' @param dl List of data frames containing data information.
#' @param target_dl List of data frames containing target information.
#' @param variable_order Order of features to be displayed in the plot.
#' @param neg_log_pval_thresh Threshold for negative log p-values.
#' @param threshold p-value threshold to plot horizontal dashed line at.
#' @param point_size Size of points in the plot.
#' @param text_size Size of text in the plot.
#' @param plot_title Title of the plot.
#' @param xints Either "outcomes" or a vector of numeric values to plot
#'  vertical lines at.
#' @param hide_x_labels If TRUE, hides x-axis labels.
#' @param domain_colours Named vector of colours for domains.
#' @return A Manhattan plot (class "gg", "ggplot") showing the association
#'  p-values of features against each solution in the provided solutions data frame,
#'  stratified by meta cluster label.
#' @export
#' @examples
#' # dl <- data_list(
#' #     list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
#' #     list(income, "household_income", "demographics", "continuous"),
#' #     list(pubertal, "pubertal_status", "demographics", "continuous"),
#' #     list(anxiety, "anxiety", "behaviour", "ordinal"),
#' #     list(depress, "depressed", "behaviour", "ordinal"),
#' #     uid = "unique_id"
#' # )
#' # 
#' # sc <- snf_config(
#' #     dl = dl,
#' #     n_solutions = 20,
#' #     min_k = 20,
#' #     max_k = 50
#' # )
#' # 
#' # sol_df <- batch_snf(dl, sc)
#' # 
#' # ext_sol_df <- extend_solutions(
#' #     sol_df,
#' #     dl = dl,
#' #     min_pval = 1e-10 # p-values below 1e-10 will be thresholded to 1e-10
#' # )
#' # 
#' # # Calculate pairwise similarities between cluster solutions
#' # sol_aris <- calc_aris(sol_df)
#' # 
#' # # Extract hierarchical clustering order of the cluster solutions
#' # meta_cluster_order <- get_matrix_order(sol_aris)
#' # 
#' # # Identify meta cluster boundaries with shiny app or trial and error
#' # # ari_hm <- meta_cluster_heatmap(sol_aris, order = meta_cluster_order)
#' # # shiny_annotator(ari_hm)
#' # 
#' # # Result of meta cluster examination
#' # split_vec <- c(2, 5, 12, 17)
#' # 
#' # ext_sol_df <- label_meta_clusters(ext_sol_df, split_vec, meta_cluster_order)
#' # 
#' # # Extracting representative solutions from each defined meta cluster
#' # rep_solutions <- get_representative_solutions(sol_aris, ext_sol_df)
#' # 
#' # mc_manhattan <- mc_manhattan_plot(
#' #     rep_solutions,
#' #     dl = dl,
#' #     point_size = 3,
#' #     text_size = 12,
#' #     plot_title = "Feature-Meta Cluster Associations",
#' #     threshold = 0.05,
#' #     neg_log_pval_thresh = 5
#' # )
#' # 
#' # mc_manhattan
mc_manhattan_plot <- function(ext_sol_df,
                              dl = NULL,
                              target_dl = NULL,
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
    # Ensure one of data list or target list is not NULL
    ###########################################################################
    if (is.null(dl) && is.null(target_dl)) {
        metasnf_error("At least one of `dl` or `target_dl` must be provided.")
    }
    ###########################################################################
    # Suppress warnings related to non-standard evaluation
    ###########################################################################
    variable <- ""
    domain <- ""
    neg_log_pval <- ""
    ###########################################################################
    # Formatting ext_sol_df as data frame
    ###########################################################################
    ext_sol_df <- data.frame(ext_sol_df)
    ###########################################################################
    # Select solution, label, and p-value related columns only
    ###########################################################################
    ext_sol_df <- gselect(ext_sol_df, c("^solution$", "^mc$", "_pval$"))
    if (ncol(ext_sol_df) == 2) {
        metasnf_error(
            "ext_sol_df is missing p-value columns. Did you",
            " provide an unextended solutions data frame instead?"
        )
    }
    ext_sol_df <- drop_cols(ext_sol_df, c("min_pval", "mean_pval", "max_pval"))
    ###########################################################################
    # Convert solution and mc to factors
    ###########################################################################
    ext_sol_df$"solution" <- factor(ext_sol_df$"solution")
    ext_sol_df$"mc" <- factor(ext_sol_df$"mc")
    if (all(is.na(ext_sol_df$"mc"))) {
        metasnf_alert(
            "Meta cluster labels not assigned. Using solution labels instead."
        )
        ext_sol_df$"mc" <- ext_sol_df$"solution"
    }
    ###########################################################################
    # Re-assign names to the data list and target list
    ###########################################################################
    if (!is.null(target_dl)) {
        dl_renamed <- dl |> dlapply(
            function(x) {
                x$"domain" <- paste0("I-", x$"domain")
                return(x)
            }
        )
        tl_renamed <- target_dl |> dlapply(
            function(x) {
                x$"domain" <- paste0("O-", x$"domain")
                return(x)
            }
        )
        dl <- c(dl_renamed, tl_renamed)
    }
    ###########################################################################
    # Columns that end with _pval are truncated by neg_log_pval_thresh
    ###########################################################################
    var_cols_idx <- endsWith(colnames(ext_sol_df), "_pval")
    var_cols <- colnames(ext_sol_df)[var_cols_idx]
    cutoff_var_cols <- ext_sol_df[, var_cols] |>
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
    ext_sol_df[, var_cols] <- cutoff_var_cols
    summary_data <- ext_sol_df |>
        tidyr::pivot_longer(
            !(c("solution", "mc")),
            names_to = "variable",
            values_to = "neg_log_pval"
        ) |>
        data.frame()
    summary_data$"variable" <- sub("_pval$", "", summary_data$"variable")
    ###########################################################################
    # Merge the summary plot with domain information from the data list
    ###########################################################################
    dl_metadata <- summary(dl, "feature") |> drop_cols("type")
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
        ggplot2::facet_grid(mc ~ .)
    ###########################################################################
    # Assigning colours to domains
    ###########################################################################
    prefixed_domains <- unique(summary_data$"domain")
    if (!is.null(target_dl)) {
        domains <- substr(prefixed_domains, 3, nchar(prefixed_domains))
    } else {
        domains <- prefixed_domains
    }
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
    if (is.null(xints) && !is.null(dl) && !is.null(target_dl)) {
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
#' @param esm Extended solutions data frame storing associations between features
#' and cluster assignments. See `?extend_solutions`.
#'
#' @param neg_log_pval_thresh Threshold for negative log p-values.
#' @param threshold P-value threshold to plot dashed line at.
#' @param point_size Size of points in the plot.
#' @param jitter_width Width of jitter.
#' @param jitter_height Height of jitter.
#' @param text_size Size of text in the plot.
#' @param plot_title Title of the plot.
#' @param hide_x_labels If TRUE, hides x-axis labels.
#' @param bonferroni_line If TRUE, plots a dashed black line at the
#'  Bonferroni-corrected equivalent of the p-value threshold.
#' @return A Manhattan plot (class "gg", "ggplot") showing the association
#'  p-values of features against each solution in the provided solutions data frame.
#' @export
#' @examples
#' # full_dl <- data_list(
#' #     list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
#' #     list(income, "household_income", "demographics", "continuous"),
#' #     list(pubertal, "pubertal_status", "demographics", "continuous"),
#' #     list(anxiety, "anxiety", "behaviour", "ordinal"),
#' #     list(depress, "depressed", "behaviour", "ordinal"),
#' #     uid = "unique_id"
#' # )
#' # 
#' # dl <- full_dl[1:3]
#' # target_dl <- full_dl[4:5]
#' # 
#' # set.seed(42)
#' # sc <- snf_config(
#' #     dl = dl,
#' #     n_solutions = 20,
#' #     min_k = 20,
#' #     max_k = 50
#' # )
#' # 
#' # sol_df <- batch_snf(dl, sc)
#' # 
#' # ext_sol_df <- extend_solutions(
#' #     sol_df,
#' #     dl = dl,
#' #     target = target_dl,
#' #     min_pval = 1e-10 # p-values below 1e-10 will be thresholded to 1e-10
#' # )
#' # 
#' # esm_manhattan <- esm_manhattan_plot(
#' #     ext_sol_df[1:5, ],
#' #     neg_log_pval_thresh = 5,
#' #     threshold = 0.05,
#' #     point_size = 3,
#' #     jitter_width = 0.1,
#' #     jitter_height = 0.1,
#' #     plot_title = "Feature-Solution Associations",
#' #     text_size = 14,
#' #     bonferroni_line = TRUE
#' # )
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
    solution <- ""
    variable <- ""
    pval <- ""
    pval_df$"solution" <- factor(pval_df$"solution")
    pval_df <- pval_df |>
        tidyr::pivot_longer(
            !solution,
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
                colour = solution
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
