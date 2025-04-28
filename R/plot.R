#' Heatmap of pairwise adjusted rand indices between solutions
#'
#' @param x Matrix of adjusted rand indices from `calc_aris()`
#' @param order Numeric vector containing row order of the heatmap.
#' @param cluster_rows Whether rows should be clustered.
#' @param cluster_columns Whether columns should be clustered.
#' @param log_graph If TRUE, log transforms the graph.
#' @param scale_diag Method of rescaling matrix diagonals. Can be "none"
#'  (don't change diagonals), "mean" (replace diagonals with average value of
#'  off-diagonals), or "zero" (replace diagonals with 0).
#' @param min_colour Colour used for the lowest value in the heatmap.
#' @param max_colour Colour used for the highest value in the heatmap.
#' @param col Colour ramp to use for the heatmap.
#' @param ... Additional parameters passed to `similarity_matrix_heatmap()`,
#'  the function that this function wraps.
#' @return Returns a heatmap (class "Heatmap" from package ComplexHeatmap)
#'  that displays the pairwise adjusted Rand indices (similarities) between
#'  the cluster solutions of the provided solutions data frame.
#' @export
#' @examples
#' \donttest{
#'     dl <- data_list(
#'         list(cort_sa, "cortical_surface_area", "neuroimaging", "continuous"),
#'         list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
#'         list(income, "household_income", "demographics", "continuous"),
#'         list(pubertal, "pubertal_status", "demographics", "continuous"),
#'         uid = "unique_id"
#'     )
#'     
#'     set.seed(42)
#'     my_sc <- snf_config(
#'         dl = dl,
#'         n_solutions = 20,
#'         min_k = 20,
#'         max_k = 50
#'     )
#'     
#'     sol_df <- batch_snf(dl, my_sc)
#'     
#'     sol_df
#'     
#'     sol_aris <- calc_aris(sol_df)
#'     
#'     meta_cluster_order <- get_matrix_order(sol_aris)
#'     
#'     # `split_vec` found by iteratively plotting ari_hm or by ?shiny_annotator()
#'     split_vec <- c(6, 10, 16)
#'     ari_hm <- plot(
#'         sol_aris,
#'         order = meta_cluster_order,
#'         split_vector = split_vec
#'     )
#' }
plot.ari_matrix <- function(x,
                            order = NULL,
                            cluster_rows = FALSE,
                            cluster_columns = FALSE,
                            log_graph = FALSE,
                            scale_diag = "none",
                            min_colour = "#282828",
                            max_colour = "firebrick2",
                            col = circlize::colorRamp2(
                                c(min(x), max(x)),
                                c(min_colour, max_colour)
                            ),
                            ...) {
    heatmap <- similarity_matrix_heatmap(
        x,
        order = order,
        cluster_rows = cluster_rows,
        cluster_columns = cluster_columns,
        log_graph = log_graph,
        scale_diag = scale_diag,
        col = col,
        column_split = NULL,
        ...
    )
    return(heatmap)
}

#' @rdname plot.ari_matrix
#' @export
meta_cluster_heatmap <- plot.ari_matrix

#' @export
plot.clust_fns_list <- function(...) {
    metasnf_error("There is no `plot` method for `clust_fns_list` class objects.")
}

#' Plot of feature values in a data list
#'
#' This plot, built on `ComplexHeatmap::Heatmap()`, visualizes the feature
#' values in a data list as a continuous heatmap with observations along the
#' columns and features along the rows.
#'
#' @param x A `data_list` object.
#' @param y Optional argument to `plot`, not used in this method.
#' @param cluster_rows Logical indicating whether to cluster the rows
#'  (observations).
#' @param cluster_columns Logical indicating whether to cluster the columns
#'  (features).
#' @param heatmap_legend_param A list of parameters for the heatmap legend.
#' @param row_title Title for the rows (observations).
#' @param column_title Title for the columns (features).
#' @param show_row_names Logical indicating whether to show row names.
#' @param ... Additional arguments passed to `ComplexHeatmap::Heatmap()`.
#' @return A heatmap visualization of feature values.
#' @export
plot.data_list <- function(x,
                           y = NULL,
                           cluster_rows = TRUE,
                           cluster_columns = TRUE,
                           heatmap_legend_param = NULL,
                           row_title = "Observation",
                           column_title = "Feature",
                           show_row_names = FALSE,
                           ...) {
    dl_df <- x |>
        as.data.frame() |>
        dplyr::select(-"uid")
    model_mat <- stats::model.matrix(~ . - 1, data = dl_df) |>
        scale()
    if (is.null(heatmap_legend_param)) {
        heatmap_legend_param <- list(
            title = "Scaled\nValue"
        )
    }
    ComplexHeatmap::Heatmap(
        model_mat,
        cluster_rows = cluster_rows,
        cluster_columns = cluster_columns,
        heatmap_legend_param = heatmap_legend_param,
        row_title = row_title,
        column_title = column_title,
        show_row_names = show_row_names,
        ...
    )
}

#' @export
plot.dist_fns_list <- function(...) {
    metasnf_error("There is no `plot` method for `dist_fns_list` class objects.")
}

#' Plot of cluster assignments in an extended solutions data frame
#'
#' This plot, built on `ComplexHeatmap::Heatmap()`, visualizes the cluster
#' assignments in a solutions data frame as a categorical heatmap with
#' observations along the columns and clusters along the rows.
#'
#' @inheritParams ComplexHeatmap::Heatmap
#' @param x An `ext_solutions_df` object.
#' @param y Optional argument to `plot`, not used in this method.
#' @param ... Additional arguments passed to `ComplexHeatmap::Heatmap()`.
#' @return A `ComplexHeatmap::Heatmap()` object visualization of cluster
#'  assignments.
#' @export
plot.ext_solutions_df <- function(x,
                                  y = NULL,
                                  cluster_rows = TRUE,
                                  cluster_columns = TRUE,
                                  show_row_names = TRUE,
                                  show_column_names = TRUE,
                                  heatmap_legend_param = NULL,
                                  row_title = "Solution",
                                  column_title = "Observation",
                                  ...) {
    if (is.null(heatmap_legend_param)) {
        heatmap_legend_param <- list(
            title = "Cluster"
        )
    }
    sol_mat <- x |>
        as.data.frame() |>
        dplyr::select(dplyr::starts_with("uid")) |>
        as.matrix()
    colnames(sol_mat) <- gsub("^uid_", "", colnames(sol_mat))
    colours <- RColorBrewer::brewer.pal(max(sol_mat), "Set3")
    names(colours) <- seq_len(max(sol_mat))
    ComplexHeatmap::Heatmap(
        sol_mat,
        cluster_rows = cluster_rows,
        cluster_columns = cluster_columns,
        col = colours,
        heatmap_legend_param = heatmap_legend_param,
        row_title = row_title,
        column_title = column_title,
        show_row_names = show_row_names,
        show_column_names = show_column_names,
        ...
    )
}


#' @export
plot.sim_mats_list <- function(...) {
    metasnf_error("There is no `plot` method for `sim_mats_list` class objects.")
}

#' Heatmap for visualizing an SNF config
#'
#' Create a heatmap where each row corresponds to a different set of 
#' hyperparameters in an SNF config object. Numeric parameters are scaled
#' normalized and non-numeric parameters are added as heatmap annotations. Rows
#' can be reordered to match prior meta clustering results.
#'
#' @inheritParams ComplexHeatmap::Heatmap
#' @param x An `snf_config` class object.
#' @param order Numeric vector indicating row ordering of SNF config.
#' @param hide_fixed Whether fixed parameters should be removed.
#' @param colours Vector of colours to use for the heatmap. Should match the
#'  length of colour_breaks.
#' @param colour_breaks Numeric vector of breaks for the legend.
#' @param column_split_vector Vector of indices to split columns by.
#' @param row_split_vector Vector of indices to split rows by.
#' @param include_weights If TRUE, includes feature weights of the weights
#'  matrix into the config heatmap.
#' @param include_settings If TRUE, includes columns from the settings data
#'  frame into the config heatmap.
#' @param ... Additional parameters passed to `ComplexHeatmap::Heatmap`.
#' @return Returns a heatmap (class "Heatmap" from package ComplexHeatmap)
#'  that displays the scaled values of the provided SNF config.
#' @export
#' @examples
#' dl <- data_list(
#'     list(income, "household_income", "demographics", "ordinal"),
#'     list(pubertal, "pubertal_status", "demographics", "continuous"),
#'     list(fav_colour, "favourite_colour", "demographics", "categorical"),
#'     list(anxiety, "anxiety", "behaviour", "ordinal"),
#'     list(depress, "depressed", "behaviour", "ordinal"),
#'     uid = "unique_id"
#' )
#' 
#' sc <- snf_config(
#'     dl,
#'     n_solutions = 10,
#'     dropout_dist = "uniform"
#' )
#' 
#' plot(sc)
plot.snf_config <- function(x,
                            order = NULL,
                            hide_fixed = FALSE,
                            show_column_names = TRUE,
                            show_row_names = TRUE,
                            rect_gp = grid::gpar(col = "black"),
                            colour_breaks = c(0, 1),
                            colours = c("black", "darkseagreen"),
                            column_split_vector = NULL,
                            row_split_vector = NULL,
                            column_split = NULL,
                            row_split = NULL,
                            column_title = NULL,
                            include_weights = TRUE,
                            include_settings = TRUE,
                            ...) {
    if (inherits(x, "snf_config")) {
        sdf <- x$"settings_df"
    } else if (inherits(x, "settings_df")) {
        sdf <- x
        include_settings <- TRUE
        include_weights <- FALSE
    } else if (inherits(x, "weights_matrix")) {
        include_settings <- FALSE
        include_weights <- TRUE
        x <- list("weights_matrix" = x)
    } else {
        print(class(x))
        metasnf_error("`x` must be an `snf_config` class object.")
    }
    if (!is.null(order)) {
        sdf <- sdf[order, ]
    }
    if (include_settings & include_weights) {
        sdf <- drop_cols(sdf, "solution")
        wm <- x$"weights_matrix"
        sdf <- cbind(sdf, as.data.frame(wm))
        trimmed_sdf <- gexclude(sdf, c("^snf_scheme$", "^clust_alg$", "dist$" ))
    } else if (include_weights) {
        wm <- x$"weights_matrix"
        sdf <- as.data.frame(wm)
        trimmed_sdf <- sdf
    } else if (include_settings) {
        sdf <- drop_cols(sdf, "solution")
        trimmed_sdf <- gexclude(sdf, c("^snf_scheme$", "^clust_alg$", "dist$" ))
    } else {
        metasnf_error(
            "At least one of `include_weights` and `include_settings` must",
            " be TRUE."
        ) 
    }
    # Scaling everything to have a max of 1
    col_maxes <- apply(trimmed_sdf, 2, function(x) 1 / max(x))
    scaled_matrix <- as.matrix(trimmed_sdf) %*% diag(col_maxes)
    colnames(scaled_matrix) <- colnames(trimmed_sdf)
    rownames(scaled_matrix) <- rownames(trimmed_sdf)
    ###########################################################################
    # Function to check number of unique values in each column
    unique_values <- apply(scaled_matrix, 2, function(x) length(unique(x)))
    fixed_columns <- colnames(scaled_matrix[, unique_values == 1])
    if (length(fixed_columns) > 0 && hide_fixed) {
        metasnf_alert(
            "Removing settings that had no variation across SNF config.\n"
        )
        scaled_matrix <- scaled_matrix[, unique_values > 1]
    }
    ###########################################################################
    # Assign splits (if any provided)
    ###########################################################################
    if (is.null(column_split_vector)) {
        inc_splits <- which(startsWith(colnames(scaled_matrix), "inc_"))
        if (length(inc_splits) > 0) {
            column_split_vector <- c(
                inc_splits[[1]], # first inclusion column
                inc_splits[[length(inc_splits)]] + 1 # last inclusion column
            )
        }
    }
    snf_param_index <- sum(c("alpha", "k", "t") %in% colnames(scaled_matrix)) + 1
    column_split_vector <- c(snf_param_index, column_split_vector)
    column_split_vector <- column_split_vector[column_split_vector < ncol(scaled_matrix)]
    if (length(column_split_vector) == 0) {
        column_split_vector <- NULL
    }
    split_results <- split_parser(
        row_split_vector = row_split_vector,
        row_split = row_split,
        column_split_vector = column_split_vector,
        column_split = column_split,
        n_rows = nrow(scaled_matrix),
        n_columns = ncol(scaled_matrix)
    )
    column_split <- split_results$"column_split"
    row_split <- split_results$"row_split"
    #--------------------------------------------------------------------------
    if (include_settings) {
        sdf$"snf_scheme" <- as.factor(sdf$"snf_scheme")
        sdf$"clust_alg" <- as.factor(sdf$"clust_alg")
        sdf$"cnt_dist" <- as.factor(sdf$"cnt_dist")
        sdf$"dsc_dist" <- as.factor(sdf$"dsc_dist")
        sdf$"ord_dist" <- as.factor(sdf$"ord_dist")
        sdf$"cat_dist" <- as.factor(sdf$"cat_dist")
        sdf$"mix_dist" <- as.factor(sdf$"mix_dist")
        clust_alg_colours <- cat_colours(sdf$"clust_alg", "Dark2")
        cnt_dist_colours <- cat_colours(sdf$"cnt_dist", "Paired")
        dsc_dist_colours <- cat_colours(sdf$"dsc_dist", "Paired")
        ord_dist_colours <- cat_colours(sdf$"ord_dist", "Paired")
        cat_dist_colours <- cat_colours(sdf$"cat_dist", "Paired")
        mix_dist_colours <- cat_colours(sdf$"mix_dist", "Paired")
        snf_scheme_colours <- c("1" = "#7fc97f", "2" = "#beaed4", "3" = "#fdc086")
        left_hm_list <- list()
        left_hm_colour_list <- list() 
        full_annotations <- list(
            list("SNF scheme", "snf_scheme", snf_scheme_colours),
            list("Clustering algorithm", "clust_alg", clust_alg_colours),
            list("Continuous metric", "cnt_dist", cnt_dist_colours),
            list("Discrete metric", "dsc_dist", dsc_dist_colours),
            list("Ordinal metric", "ord_dist", ord_dist_colours),
            list("Categorical metric", "cat_dist", cat_dist_colours),
            list("Mixed metric", "mix_dist", mix_dist_colours)
        )
        for (set in full_annotations) {
            if (hide_fixed) {
                if (length(set[[3]]) > 1) {
                    left_hm_list <- c(left_hm_list, set[[2]])
                    names(left_hm_list)[length(left_hm_list)] <- set[[1]]
                    left_hm_colour_list <- c(left_hm_colour_list, list(set[[3]]))
                    names(left_hm_colour_list)[length(left_hm_colour_list)] <- set[[1]]
                }
            } else {
                left_hm_list <- c(left_hm_list, set[[2]])
                names(left_hm_list)[length(left_hm_list)] <- set[[1]]
                left_hm_colour_list <- c(left_hm_colour_list, list(set[[3]]))
                names(left_hm_colour_list)[length(left_hm_colour_list)] <- set[[1]]
            }
        }
        annotations_list <- generate_annotations_list(
            df = sdf,
            left_hm = left_hm_list,
            annotation_colours = left_hm_colour_list
        )
    } else {
        annotations_list <- NULL
    }
    ###########################################################################
    heatmap <- ComplexHeatmap::Heatmap(
        scaled_matrix,
        cluster_rows = FALSE,
        cluster_columns = FALSE,
        rect_gp = rect_gp,
        col = circlize::colorRamp2(colour_breaks, colours),
        heatmap_legend_param = list(
            color_bar = "continuous",
            title = "Normalized\nSetting",
            at = colour_breaks
        ),
        row_split = row_split,
        column_split = column_split,
        column_title = column_title,
        left_annotation = annotations_list$"left_annotations",
        ...
    )
    return(heatmap)
}

#' @rdname plot.snf_config
#' @export
config_heatmap <- plot.snf_config

#' @rdname plot.snf_config
#' @export
plot.settings_df <- plot.snf_config

#' @rdname plot.snf_config
#' @export
plot.weights_matrix <- plot.snf_config

#' Plot of cluster assignments in a solutions data frame
#'
#' This plot, built on `ComplexHeatmap::Heatmap()`, visualizes the cluster
#' assignments in a solutions data frame as a categorical heatmap with
#' observations along the columns and clusters along the rows.
#'
#' @inheritParams ComplexHeatmap::Heatmap
#' @param x A `solutions_df` object.
#' @param y Optional argument to `plot`, not used in this method.
#' @param ... Additional arguments passed to `ComplexHeatmap::Heatmap()`.
#' @return A `ComplexHeatmap::Heatmap()` object visualization of cluster
#'  assignments.
#' @export
plot.solutions_df <- function(x,
                              y = NULL,
                              cluster_rows = FALSE,
                              cluster_columns = TRUE,
                              heatmap_legend_param = NULL,
                              row_title = "Solution",
                              column_title = "Observation",
                              ...) {
    if (is.null(heatmap_legend_param)) {
        heatmap_legend_param <- list(
            title = "Cluster"
        )
    }
    sol_mat <- x |>
        as.data.frame() |>
        dplyr::select(dplyr::starts_with("uid")) |>
        as.matrix()
    colnames(sol_mat) <- gsub("^uid_", "", colnames(sol_mat))
    colours <- RColorBrewer::brewer.pal(max(sol_mat), "Set3")
    names(colours) <- seq_len(max(sol_mat))
    ComplexHeatmap::Heatmap(
        sol_mat,
        cluster_rows = cluster_rows,
        cluster_columns = cluster_columns,
        col = colours,
        heatmap_legend_param = heatmap_legend_param,
        row_title = row_title,
        column_title = column_title,
        ...
    )
}

#' @rdname plot.solutions_df
#' @export
plot.t_solutions_df <- function(x, ...) {
    sol_df <- t(x)
    plot(sol_df, ...)
}

#' @rdname plot.ext_solutions_df
#' @export
plot.t_ext_solutions_df <- function(x, ...) {
    ext_sol_df <- t(x)
    plot(ext_sol_df, ...)
}
