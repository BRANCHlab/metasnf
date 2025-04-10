#' Alluvial plot of patients across cluster counts and important features
#'
#' This function creates an alluvial plot that shows how observations in
#' a similarity matrix could have been clustered over a set of clustering
#' functions.
#'
#' @param cluster_sequence A list of clustering algorithms.
#' @param similarity_matrix A similarity matrix.
#' @param dl A data list.
#' @param data A data frame that contains any features to include in the plot.
#' @param key_outcome The name of the feature that determines how each patient
#'  stream is coloured in the alluvial plot.
#' @param key_label Name of key outcome to be used for the plot legend.
#' @param title Title of the plot.
#' @param extra_outcomes Names of additional features to add to the plot.
#' @return An alluvial plot (class "gg" and "ggplot") showing distribution of
#' a feature across varying number cluster solutions.
#' @export
#' @examples
#' input_dl <- data_list(
#'     list(gender_df, "gender", "demographics", "categorical"),
#'     list(diagnosis_df, "diagnosis", "clinical", "categorical"),
#'     uid = "patient_id"
#' )
#' 
#' sc <- snf_config(input_dl, n_solutions = 1)
#' 
#' sol_df <- batch_snf(input_dl, sc, return_sim_mats = TRUE)
#' 
#' sim_mats <- sim_mats_list(sol_df)
#' 
#' clust_fn_sequence <- list(spectral_two, spectral_four)
#' 
#' alluvial_cluster_plot(
#'     cluster_sequence = clust_fn_sequence,
#'     similarity_matrix = sim_mats[[1]],
#'     dl = input_dl,
#'     key_outcome = "gender",
#'     key_label = "Gender",
#'     extra_outcomes = "diagnosis",
#'     title = "Gender Across Cluster Counts"
#' )
alluvial_cluster_plot <- function(cluster_sequence,
                                  similarity_matrix,
                                  dl = NULL,
                                  data = NULL,
                                  key_outcome,
                                  key_label = key_outcome,
                                  extra_outcomes = NULL,
                                  title = NULL) {
    ###########################################################################
    # Dismissing the "no visible binding" problem during building
    ###########################################################################
    x <- ""
    frequency <- ""
    stratum <- ""
    count <- ""
    fill <- ""
    ###########################################################################
    # Calculate the cluster solutions for each cluster algorithm provided
    ###########################################################################
    alluvial_df <- data.frame(uid = colnames(similarity_matrix))
    for (algorithm in cluster_sequence) {
        solution <- algorithm(similarity_matrix)
        nclust <- length(unique(solution))
        solution_col <- data.frame(solution)
        colnames(solution_col) <- paste0("c", nclust)
        alluvial_df <- cbind(alluvial_df, solution_col)
    }
    ###########################################################################
    # Isolate feature of interest
    ###########################################################################
    data <- assemble_data(data = data, dl = dl)
    outcome_df_cols <- c("uid", key_outcome, extra_outcomes)
    outcome_df <- data[, colnames(data) %in% outcome_df_cols]
    alluvial_df <- dplyr::inner_join(
        alluvial_df,
        outcome_df,
        by = "uid"
    )
    alluvial_df <- alluvial_df |>
        drop_cols("uid") |>
        dplyr::group_by(dplyr::across(0:(ncol(alluvial_df) - 1))) |>
        dplyr::summarize("frequency" = dplyr::n(), .groups = "keep") |>
        data.frame()
    n_alluvial_columns <- length(cluster_sequence) + 1 + length(extra_outcomes)
    alluvial_indices <- 1:n_alluvial_columns
    alluvial_df$"fill" <- alluvial_df[, key_outcome]
    # change to lode form
    alluvial_df_lodes <- ggalluvial::to_lodes_form(
        alluvial_df,
        axes = alluvial_indices,
        id = "count"
    )
    ###########################################################################
    # Plotting
    ###########################################################################
    plot <- ggplot2::ggplot(
        alluvial_df_lodes,
        ggplot2::aes(
            x = x,
            y = frequency,
            stratum = stratum,
            alluvium = count
        )
    ) +
        ggalluvial::geom_alluvium(
            ggplot2::aes(
                fill = fill,
                color = fill
            )
        ) +
        ggalluvial::geom_stratum(width = 1 / 4) +
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
            title = title,
            y = "Frequency"
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
