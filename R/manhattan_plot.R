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
        if (!is.null(bonferroni_line)) {
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
