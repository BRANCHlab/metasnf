#' Manhattan plot of p-values
#'
#' @param pvals_df Output of the pval_select() function
#'
#' @export
manhattan_plot <- function(pvals_df) {
    # Suppress global visible binding errors during building
    row_id <- ""
    variable <- ""
    p_value <- ""
    var_cols <- 2:ncol(pvals_df)
    pvals_df[, var_cols] <- pvals_df[, var_cols] |>
        apply(
            MARGIN = 2,
            FUN = function(x) {
                -log10(x)
            }
        )
    pvals_df$"row_id" <- factor(pvals_df$"row_id")
    pvals_df <- pvals_df |>
        tidyr::pivot_longer(
            !row_id,
            names_to = "variable",
            values_to = "p_value"
        ) |>
        data.frame()
    plot <- pvals_df |> ggplot2::ggplot() +
        ggplot2::geom_point(
            mapping = ggplot2::aes(
                x = variable,
                y = p_value,
                colour = row_id
            ),
            alpha = 1,
            size = 5
        ) +
        ggplot2::geom_hline(
            yintercept = -log10(0.05 / nlevels(pvals_df$"row_id")),
            linetype = "dashed",
            colour = "black"
        ) +
        ggplot2::geom_hline(
            yintercept = -log10(0.05),
            linetype = "dashed",
            colour = "red"
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
    return(plot)
}
