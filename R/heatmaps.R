#' Plot heatmap of similarity matrix
#'
#' @param similarity_matrix A similarity matrix
#'
#' @param order Vector of numbers to reorder the similarity matrix (and data
#' if provided). Overwrites ordering specified by cluster_solution param.
#'
#' @param cluster_solution Vector containing cluster assignments.
#'
#' @param scale_diag Method of rescaling matrix diagonals. Can be "none"
#' (don't change diagonals), "mean" (replace diagonals with average value of
#' off-diagonals), or "zero" (replace diagonals with 0).
#'
#' @param log_graph If TRUE, log transforms the graph.
#'
#' @param cluster_rows Parameter for ComplexHeatmap::Heatmap.
#'
#' @param cluster_columns Parameter for ComplexHeatmap::Heatmap.
#'
#' @param show_row_names Parameter for ComplexHeatmap::Heatmap.
#'
#' @param show_column_names Parameter for ComplexHeatmap::Heatmap.
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @param data A dataframe containing elements requested for annotation.
#'
#' @param left_bar Named list of strings, where the strings are features in
#' df that should be used for a barplot annotation on the left of the plot and
#' the names are the names that will be used to caption the plots and their
#' legends.
#'
#' @param left_hm Like left_bar, but with a heatmap annotation instead of a
#' barplot annotation.
#'
#' @param right_bar See left_bar.
#'
#' @param top_bar See left_bar.
#'
#' @param bottom_bar See left_bar.
#'
#' @param right_hm See left_hm.
#'
#' @param top_hm See left_hm.
#'
#' @param bottom_hm See left_hm.
#'
#' @param annotation_colours Named list of heatmap annotations and their
#'  colours.
#'
#' @param min_colour Colour used for the lowest value in the heatmap.
#'
#' @param max_colour Colour used for the highest value in the heatmap.
#'
#' @param split_vector A vector of partition indices.
#'
#' @param row_split Standard parameter of `ComplexHeatmap::Heatmap`.
#'
#' @param column_split Standard parameter of `ComplexHeatmap::Heatmap`.
#'
#' @param ... Additional parameters passed into ComplexHeatmap::Heatmap.
#'
#' @return Returns a heatmap (class "Heatmap" from package ComplexHeatmap)
#' that displays the similarities between observations in the provided matrix.
#'
#' @export
similarity_matrix_heatmap <- function(similarity_matrix,
                                      order = NULL,
                                      cluster_solution = NULL,
                                      scale_diag = "mean",
                                      log_graph = TRUE,
                                      cluster_rows = FALSE,
                                      cluster_columns = FALSE,
                                      show_row_names = FALSE,
                                      show_column_names = FALSE,
                                      data_list = NULL,
                                      data = NULL,
                                      left_bar = NULL,
                                      right_bar = NULL,
                                      top_bar = NULL,
                                      bottom_bar = NULL,
                                      left_hm = NULL,
                                      right_hm = NULL,
                                      top_hm = NULL,
                                      bottom_hm = NULL,
                                      annotation_colours = NULL,
                                      min_colour = NULL,
                                      max_colour = NULL,
                                      split_vector = NULL,
                                      row_split = NULL,
                                      column_split = NULL,
                                      ...) {
    check_hm_dependencies()
    ###########################################################################
    # Assemble any provided data
    ###########################################################################
    data <- assemble_data(data, data_list)
    ###########################################################################
    # Ensure that annotations aren't being requested when data isn't given
    ###########################################################################
    check_dataless_annotations(
        list(
            left_bar,
            right_bar,
            top_bar,
            bottom_bar,
            left_hm,
            right_hm,
            top_hm,
            bottom_hm
        ),
        data
    )
    ###########################################################################
    # Sort the matrix and any other provided data
    ###########################################################################
    # The order can come from the following sources:
    #  - nowhere (reorder by similarity?)
    #  - cluster_solution (just sort by cluster_solution)
    #  - hard specified (sort the similarity_matrix by the order provided)
    if (is.null(order)) {
        # Order was not provided
        if (!is.null(cluster_solution)) {
            # Cluster solution was provided
            order <- sort(cluster_solution, index.return = TRUE)$"ix"
            message("Sorting by cluster solution.")
        } else {
            # Neither order nor cluster solution was provided
            order <- seq_len(nrow(similarity_matrix))
        }
    } else {
        message("Sorting by order.")
    }
    similarity_matrix <- similarity_matrix[order, order]
    data <- data[order, , drop = FALSE]
    ###########################################################################
    # Log the graph if requested
    if (log_graph) {
        similarity_matrix <- log(similarity_matrix)
        title <- "log(Similarity)"
    } else {
        title <- "Similarity"
    }
    ###########################################################################
    # Re-scale diagonals
    similarity_matrix <- scale_diagonals(
        similarity_matrix,
        method = scale_diag
    )
    ###########################################################################
    # Assign breaks in the legend
    minimum <- min(similarity_matrix) |> signif(2)
    maximum <- max(similarity_matrix) |> signif(2)
    middle <- mean(c(minimum, maximum)) |> signif(2)
    ###########################################################################
    # Generate annotations
    annotations_list <- generate_annotations_list(
        df = data,
        left_hm = left_hm,
        right_hm = right_hm,
        top_hm = top_hm,
        bottom_hm = bottom_hm,
        left_bar = left_bar,
        right_bar = right_bar,
        top_bar = top_bar,
        bottom_bar = bottom_bar,
        annotation_colours = annotation_colours
    )
    args_list <- list(...)
    if (is.null(args_list$"top_annotation")) {
        args_list$"top_annotation" <- annotations_list$"top_annotations"
    }
    if (is.null(args_list$"left_annotation")) {
        args_list$"left_annotation" <- annotations_list$"left_annotations"
    }
    if (is.null(args_list$"right_annotation")) {
        args_list$"right_annotation" <- annotations_list$"right_annotations"
    }
    if (is.null(args_list$"bottom_annotation")) {
        args_list$"bottom_annotation" <- annotations_list$"bottom_annotations"
    }
    ###########################################################################
    if (!is.null(min_colour) && !is.null(max_colour)) {
        args_list$"col" <- circlize::colorRamp2(
            c(min(similarity_matrix), max(similarity_matrix)),
            c(min_colour, max_colour)
        )
    }
    ###########################################################################
    # Assign splits (if any provided)
    ###########################################################################
    split_results <- split_parser(
        row_split_vector = split_vector,
        row_split = row_split,
        column_split_vector = split_vector,
        column_split = column_split,
        n_rows = nrow(similarity_matrix),
        n_columns = ncol(similarity_matrix)
    )
    args_list$"row_split" <- split_results$"row_split"
    args_list$"column_split" <- split_results$"column_split"
    ###########################################################################
    # Build plot
    args_list$"matrix" <- similarity_matrix
    args_list$"cluster_rows" <- cluster_rows
    args_list$"cluster_columns" <- cluster_columns
    args_list$"show_row_names" <- show_row_names
    args_list$"show_column_names" <- show_column_names
    args_list$"heatmap_legend_param" <- list(
        color_bar = "continuous",
        title = title,
        at = c(minimum, middle, maximum)
    )
    heatmap <- suppressMessages(
        do.call(
            ComplexHeatmap::Heatmap,
            args_list
        )
    )
    return(heatmap)
}

#' Heatmap of pairwise adjusted rand indices between solutions
#'
#' @param aris Matrix of adjusted rand indices from `calc_aris()`
#'
#' @param order Numeric vector containing row order of the heatmap.
#'
#' @param cluster_rows Whether rows should be clustered.
#'
#' @param cluster_columns Whether columns should be clustered.
#'
#' @param log_graph If TRUE, log transforms the graph.
#'
#' @param scale_diag Method of rescaling matrix diagonals. Can be "none"
#' (don't change diagonals), "mean" (replace diagonals with average value of
#' off-diagonals), or "zero" (replace diagonals with 0).
#'
#' @param min_colour Colour used for the lowest value in the heatmap.
#'
#' @param max_colour Colour used for the highest value in the heatmap.
#'
#' @param col Colour ramp to use for the heatmap.
#'
#' @param ... Additional parameters passed to `similarity_matrix_heatmap()`,
#' the function that this function wraps.
#'
#' @return Returns a heatmap (class "Heatmap" from package ComplexHeatmap)
#' that displays the pairwise adjusted Rand indices (similarities) between
#' the cluster solutions of the provided solutions matrix.
#'
#' @export
adjusted_rand_index_heatmap <- function(aris,
                                        order = NULL,
                                        cluster_rows = FALSE,
                                        cluster_columns = FALSE,
                                        log_graph = FALSE,
                                        scale_diag = "none",
                                        min_colour = "#282828",
                                        max_colour = "firebrick2",
                                        col = circlize::colorRamp2(
                                            c(min(aris), max(aris)),
                                            c(min_colour, max_colour)
                                        ),
                                        ...) {
    heatmap <- similarity_matrix_heatmap(
        aris,
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

#' Heatmap of pairwise associations between features
#'
#' @param correlation_matrix Matrix containing all pairwise association
#' p-values. The recommended way to obtain this matrix is through the
#' calc_assoc_pval function.
#'
#' @param scale_diag Parameter that controls how the diagonals of the
#' correlation_matrix are adjusted in the heatmap. For best viewing, this is
#' set to "max", which will match the diagonals to whichever pairwise
#' association has the highest p-value.
#'
#' @param cluster_rows Parameter for ComplexHeatmap::Heatmap. Will be ignored
#' if split_by_domain is also provided.
#'
#' @param cluster_columns Parameter for ComplexHeatmap::Heatmap. Will be
#' ignored if split_by_domain is also provided.
#'
#' @param show_row_names Parameter for ComplexHeatmap::Heatmap.
#'
#' @param show_column_names Parameter for ComplexHeatmap::Heatmap.
#'
#' @param show_heatmap_legend Parameter for ComplexHeatmap::Heatmap.
#'
#' @param confounders A named list where the elements are columns in the
#' correlation_matrix and the names are the corresponding display names.
#'
#' @param out_of_models Like confounders, but a named list of out of model
#' measures (who are also present as columns in the correlation_matrix).
#'
#' @param annotation_colours Named list of heatmap annotations and their
#'  colours.
#'
#' @param labels_colour Vector of colours to use for the columns and rows
#' of the heatmap.
#'
#' @param split_by_domain The results of `dl_var_summar` - a dataframe that has
#' the domain of every feature in the plotted data.
#' columns of the correlation_matrix. Will be used to "slice" the heatmap into
#' visually separated sections.
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @param significance_stars If TRUE (default), plots significance stars on
#' heatmap cells
#'
#' @param slice_font_size Font size for domain separating labels.
#'
#' @param ... Additional parameters passed into ComplexHeatmap::Heatmap.
#'
#' @return Returns a heatmap (class "Heatmap" from package ComplexHeatmap)
#' that displays the pairwise associations between features from the provided
#' correlation_matrix.
#'
#' @export
assoc_pval_heatmap <- function(correlation_matrix,
                               scale_diag = "max",
                               cluster_rows = TRUE,
                               cluster_columns = TRUE,
                               show_row_names = TRUE,
                               show_column_names = TRUE,
                               show_heatmap_legend = FALSE,
                               confounders = NULL,
                               out_of_models = NULL,
                               annotation_colours = NULL,
                               labels_colour = NULL,
                               split_by_domain = FALSE,
                               data_list = NULL,
                               significance_stars = TRUE,
                               slice_font_size = 8,
                               ...) {
    check_hm_dependencies()
    ###########################################################################
    # Format data
    ###########################################################################
    confounder_names <- confounders |> unlist() |> as.character()
    out_of_models_names <- out_of_models |> unlist() |> as.character()
    peripheral_names <- c(confounder_names, out_of_models_names)
    peripheral_idx <- colnames(correlation_matrix) %in% peripheral_names
    peripheral_data <- correlation_matrix[!peripheral_idx, peripheral_idx]
    correlation_matrix <- correlation_matrix[!peripheral_idx, !peripheral_idx]
    ###########################################################################
    if (is.null(labels_colour)) {
        labels_colour <- c(rep("black", ncol(correlation_matrix)))
        names(labels_colour) <- colnames(correlation_matrix)
    }
    title <- "p-value"
    ###########################################################################
    # Scale the matrix diagonals
    ###########################################################################
    correlation_matrix <- scale_diagonals(
        correlation_matrix,
        method = scale_diag
    )
    # Lower threshold on p-values
    correlation_matrix[correlation_matrix < 0.00001] <- 0.00001
    # Assign breaks in the legend
    minimum <- min(correlation_matrix) |> signif(2)
    maximum <- max(correlation_matrix) |> signif(2)
    middle <- mean(c(minimum, maximum)) |> signif(2)
    ###########################################################################
    # Prepare the annotations
    ###########################################################################
    in_model_colours <- c(
        "black",
        "navy",
        "blue",
        "royalblue",
        "steelblue2"
    )
    confounder_colour_values <- c(
        "black",
        "red4",
        "red2",
        "red",
        "lightcoral"
    )
    confounder_colours <- circlize::colorRamp2(
        c(0, 0.0005, 0.005, 0.05, 1),
        confounder_colour_values
    )
    out_of_model_colour_values <- c(
        "black",
        "darkgreen",
        "forestgreen",
        "chartreuse4",
        "darkseagreen1"
    )
    out_of_model_colours <- circlize::colorRamp2(
        c(0, 0.0005, 0.005, 0.05, 1),
        out_of_model_colour_values
    )
    clist <- names(confounders) |>
        lapply(
            function(x) {
                confounder_colours
            }
        )
    names(clist) <- names(confounders)
    olist <- names(out_of_models) |>
        lapply(
            function(x) {
                out_of_model_colours
            }
        )
    names(olist) <- names(out_of_models)
    annotation_colours <- c(clist, olist)
    annotations_list <- generate_annotations_list(
        df = data.frame(peripheral_data),
        left_hm = confounders,
        top_hm = out_of_models,
        annotation_colours = annotation_colours,
        show_legend = FALSE
    )
    ###########################################################################
    # Combining all the arguments to provide to ComplexHeatmap()
    ###########################################################################
    args_list <- list(...)
    if (is.null(args_list$"top_annotation")) {
        args_list$"top_annotation" <- annotations_list$"top_annotations"
    }
    if (is.null(args_list$"left_annotation")) {
        args_list$"left_annotation" <- annotations_list$"left_annotations"
    }
    args_list$"matrix" <- correlation_matrix
    args_list$"cluster_rows" <- cluster_rows
    args_list$"cluster_columns" <- cluster_columns
    args_list$"show_row_names" <- show_row_names
    args_list$"show_column_names" <- show_column_names
    args_list$"heatmap_legend_param" <- list(
        color_bar = "continuous",
        title = title,
        at = c(minimum, middle, maximum)
    )
    args_list$"col" <- circlize::colorRamp2(
        c(0, 0.0005, 0.005, 0.05, 1),
        in_model_colours
    )
    args_list$"column_names_gp" <- grid::gpar(
        fontsize = 9,
        col = labels_colour
    )
    args_list$"row_names_gp" <- grid::gpar(
        fontsize = 9,
        col = labels_colour
    )
    args_list$"show_heatmap_legend" <- show_heatmap_legend
    if (significance_stars) {
        args_list$"cell_fun" <- cell_significance_fn(correlation_matrix)
    }
    if (split_by_domain) {
        if (is.null(data_list)) {
            stop(
                "You must provide a data_list to split the heatmap by domain."
            )
        }
        dl_var_summary <- dl_variable_summary(data_list)
        keep_vars <- dl_var_summary$"name" %in% colnames(correlation_matrix)
        dl_var_summary <- dl_var_summary[keep_vars, ]
        args_list$"cluster_rows" <- FALSE
        args_list$"cluster_columns" <- FALSE
        args_list$"row_split" <- factor(dl_var_summary$"domain")
        args_list$"column_split" <- factor(dl_var_summary$"domain")
        args_list$"row_title_gp" <- grid::gpar(fontsize = slice_font_size)
        args_list$"column_title_gp" <- grid::gpar(fontsize = slice_font_size)
    }
    ###########################################################################
    # Create heatmap
    ###########################################################################
    heatmap <- suppressMessages(
        do.call(
            ComplexHeatmap::Heatmap,
            args_list
        )
    )
    ###########################################################################
    # Add annotation legends
    ###########################################################################
    lgd_list <- list(
        ComplexHeatmap::Legend(
            labels = c(
                "p = 1",
                "p < 0.05",
                "p < 0.005",
                "p < 0.0005",
                "p = 0"
            ),
            title = "Association p-values",
            type = "grid",
            legend_gp = grid::gpar(
                fill = rev(in_model_colours)
            ),
        )
    )
    if (!is.null(confounders)) {
        lgd_list[[length(lgd_list) + 1]] <- ComplexHeatmap::Legend(
            labels = c(
                "p = 1",
                "p < 0.05",
                "p < 0.005",
                "p < 0.0005",
                "p = 0"
            ),
            title = "Confounder p-values",
            type = "grid",
            legend_gp = grid::gpar(
                fill = rev(confounder_colour_values)
            )
        )
    }
    if (!is.null(out_of_models)) {
        lgd_list[[length(lgd_list) + 1]] <- ComplexHeatmap::Legend(
            labels = c(
                "p = 1",
                "p < 0.05",
                "p < 0.005",
                "p < 0.0005",
                "p = 0"
            ),
            title = "Out-of-Model p-values",
            type = "grid",
            legend_gp = grid::gpar(
                fill = rev(out_of_model_colour_values)
            )
        )
    }
    heatmap <- ComplexHeatmap::draw(
        heatmap,
        annotation_legend_list = lgd_list
    )
}

#' Heatmap for visualizing a settings matrix
#'
#' Scales settings matrix values between 0 and 1 and plots as a heatmap. Rows
#' can be reordered to match prior meta clustering results.
#'
#' @param settings_matrix Matrix indicating parameters to iterate SNF through.
#'
#' @param remove_fixed_columns Whether columns that have no variation should be
#' removed.
#'
#' @param order Numeric vector indicating row ordering of settings matrix.
#'
#' @param show_column_names Whether column names should be shown.
#'
#' @param show_row_names Whether row names should be shown.
#'
#' @param rect_gp Cell border function for `ComplexHeatmap::Heatmap`.
#'
#' @param column_title Standard parameter of `ComplexHeatmap::Heatmap`.
#'
#' @param colour_breaks Numeric vector of breaks for the legend.
#'
#' @param colours Vector of colours to use for the heatmap. Should match the
#' length of colour_breaks.
#'
#' @param column_split_vector Vector of indices to split columns by.
#'
#' @param column_split Standard parameter of `ComplexHeatmap::Heatmap`.
#'
#' @param row_split_vector Vector of indices to split rows by.
#'
#' @param row_split Standard parameter of `ComplexHeatmap::Heatmap`.
#'
#' @param ... Additional parameters passed to `ComplexHeatmap::Heatmap`.
#'
#' @return Returns a heatmap (class "Heatmap" from package ComplexHeatmap)
#' that displays the scaled values of the provided settings matrix.
#'
#' @export
settings_matrix_heatmap <- function(settings_matrix,
                                    order = NULL,
                                    remove_fixed_columns = TRUE,
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
                                    ...) {
    if (!is.null(order)) {
        settings_matrix <- settings_matrix[order, ]
    }
    # Scaling everything to have a max of 1
    col_maxes <- apply(settings_matrix, 2, function(x) 1 / max(x))
    scaled_matrix <- as.matrix(settings_matrix) %*% diag(col_maxes)
    colnames(scaled_matrix) <- colnames(settings_matrix)
    rownames(scaled_matrix) <- rownames(settings_matrix)
    ###########################################################################
    # Function to check number of unique values in each column
    unique_values <- apply(scaled_matrix, 2, function(x) length(unique(x)))
    fixed_columns <- colnames(scaled_matrix[, unique_values == 1])
    if (length(fixed_columns) > 0 && remove_fixed_columns) {
        message(
            "Removing columns that had no variation across settings matrix: \n",
            paste(
                paste0(seq_along(fixed_columns), ". ", fixed_columns),
                collapse = "\n "
            )
        )
        scaled_matrix <- scaled_matrix[, unique_values > 1]
    }
    ###########################################################################
    # Assign splits (if any provided)
    ###########################################################################
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
    ###########################################################################
    heatmap <- ComplexHeatmap::Heatmap(
        scaled_matrix,
        cluster_rows = FALSE,
        cluster_columns = FALSE,
        rect_gp = rect_gp,
        col = circlize::colorRamp2(colour_breaks, colours),
        heatmap_legend_param = list(
            color_bar = "continuous",
            title = "Scaled Setting",
            at = colour_breaks
        ),
        row_split = row_split,
        column_split = column_split,
        column_title = column_title,
        ...
    )
    return(heatmap)
}

#' Heatmap of p-values
#'
#' @param pvals A matrix of p-values.
#'
#' @param order Numeric vector containing row order of the heatmap.
#'
#' @param cluster_columns Whether columns should be sorted by hierarchical
#' clustering.
#'
#' @param cluster_rows Whether rows should be sorted by hierarchical
#' clustering.
#'
#' @param show_column_names Whether column names should be shown.
#'
#' @param min_colour Colour used for the lowest value in the heatmap.
#'
#' @param mid_colour Colour used for the middle value in the heatmap.
#'
#' @param max_colour Colour used for the highest value in the heatmap.
#'
#' @param legend_breaks Numeric vector of breaks for the legend.
#'
#' @param show_row_names Whether row names should be shown.
#'
#' @param col Colour function for `ComplexHeatmap::Heatmap()`
#'
#' @param heatmap_legend_param Legend function for `ComplexHeatmap::Heatmap()`
#'
#' @param rect_gp Cell border function for `ComplexHeatmap::Heatmap()`
#'
#' @param row_split_vector Vector of indices to split rows by.
#'
#' @param column_split_vector Vector of indices to split columns by.
#'
#' @param row_split Standard parameter of `ComplexHeatmap::Heatmap`.
#'
#' @param column_split Standard parameter of `ComplexHeatmap::Heatmap`.
#'
#' @param ... Additional parameters passed to `ComplexHeatmap::Heatmap`.
#'
#' @return Returns a heatmap (class "Heatmap" from package ComplexHeatmap)
#' that displays the provided p-values.
#'
#' @export
pval_heatmap <- function(pvals,
                         order = NULL,
                         cluster_columns = TRUE,
                         cluster_rows = FALSE,
                         show_row_names = FALSE,
                         show_column_names = TRUE,
                         min_colour = "red2",
                         mid_colour = "lightyellow",
                         max_colour = "slateblue4",
                         legend_breaks = c(0, 0.5, 1),
                         col = circlize::colorRamp2(
                             legend_breaks,
                             c(min_colour, mid_colour, max_colour)
                         ),
                         heatmap_legend_param = list(
                             color_bar = "continuous",
                             title = "p-value",
                             at = c(0, 0.5, 1)
                         ),
                         rect_gp = grid::gpar(col = "black"),
                         column_split_vector = NULL,
                         row_split_vector = NULL,
                         column_split = NULL,
                         row_split = NULL,
                         ...) {
    if ("row_id" %in% colnames(pvals)) {
        rownames(pvals) <- pvals$"row_id"
        pvals <- pvals |>
            dplyr::select(-"row_id")
    }
    if (!is.null(order)) {
        pvals <- pvals[order, ]
    }
    ###########################################################################
    # Assign splits (if any provided)
    ###########################################################################
    split_results <- split_parser(
        row_split_vector = row_split_vector,
        row_split = row_split,
        column_split_vector = column_split_vector,
        column_split = column_split,
        n_rows = nrow(pvals),
        n_columns = ncol(pvals)
    )
    row_split <- split_results$"row_split"
    column_split <- split_results$"column_split"
    ###########################################################################
    heatmap <- ComplexHeatmap::Heatmap(
        as.matrix(pvals),
        cluster_columns = cluster_columns,
        cluster_rows = cluster_rows,
        show_row_names = show_row_names,
        show_column_names = show_column_names,
        col = col,
        heatmap_legend_param = heatmap_legend_param,
        rect_gp = rect_gp,
        row_split = row_split,
        column_split = column_split,
        ...
    )
    return(heatmap)
}

#' Launch shiny app to identify meta cluster boundaries
#'
#' @param ari_heatmap Heatmap of ARIs to divide into meta clusters.
#'
#' @return Does not return any value. Launches interactive shiny applet.
#'
#' @export
shiny_annotator <- function(ari_heatmap) {
    drawn_heatmap <- ComplexHeatmap::draw(ari_heatmap)
    InteractiveComplexHeatmap::htShiny(
        drawn_heatmap,
        response = "click",
        title = "Meta Cluster Identification",
        description = paste0(
            "Click on the heatmap to identify the indices of the meta cluster",
            " boundaries. You can recreate the similarity matrix heatmap",
            " passing these values as the `split_vector` argument to have the",
            " meta clusters visually separated and labeled. For example,",
            " if the boundaries of the meta clusters were at row/column",
            " indices 150, 300, and 313, use the argument",
            " `split_vector = c(150, 300, 313) when recreating the heatmap."
        )
    )
}

#' Place significance stars on ComplexHeatmap cells.
#'
#' This is an internal function meant to be used to by the
#' assoc_pval_heatmap function.
#'
#' @param data The matrix containing the cells to base the significance stars
#' on.
#'
#' @return cell_fn Another function that is well-formatted for usage as the
#' cell_fun argument in ComplexHeatmap::Heatmap.
#'
#' @export
cell_significance_fn <- function(data) {
    cell_fn <- function(j, i, x, y, width, height, fill) {
        flag <- 0
        if (data[i, j] < 0.0001) {
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
        if (flag == 0 && data[i, j] < 0.001) {
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
        if (flag == 0 && data[i, j] < 0.01) {
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
    }
    return(cell_fn)
}

#' Collapse a dataframe and/or a data_list into a single dataframe
#'
#' @param data A dataframe.
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @return A class "data.frame" object containing all the features of the
#' provided data frame and/or data list.
#'
#' @export
assemble_data <- function(data, data_list) {
    if (!is.null(data_list)) {
        merged_df <- collapse_dl(data_list)
    }
    if (is.null(data)) {
        if (!is.null(data_list)) {
            # User didn't provide data, but did provide data list
            data <- merged_df
        }
    } else {
        if (!is.null(data_list)) {
            # User provided both the data and the data_list, so merge them
            data <- dplyr::inner_join(data, merged_df, by = "subjectkey")
        }
    }
    return(data.frame(data))
}

#' Generate annotations list
#'
#' Intermediate function that takes in formatted lists of features and the
#'  annotations they should be viewed through and returns annotation objects
#'  usable by ComplexHeatmap::Heatmap.
#'
#' @param df Dataframe containing all the data that is specified in the
#'  remaining arguments.
#' @param left_bar Named list of strings, where the strings are features in
#'  df that should be used for a barplot annotation on the left of the plot and
#'  the names are the names that will be used to caption the plots and their
#'  legends.
#' @param left_hm Like left_bar, but with a heatmap annotation instead of a
#'  barplot annotation.
#' @param right_bar See left_bar.
#' @param top_bar See left_bar.
#' @param bottom_bar See left_bar.
#' @param right_hm See left_hm.
#' @param top_hm See left_hm.
#' @param bottom_hm See left_hm.
#' @param show_legend Add legends to the annotations.
#' @param annotation_colours Named list of heatmap annotations and their
#'  colours.
#'
#' @return annotations_list A named list of all the annotations.
#'
#' @export
generate_annotations_list <- function(df,
                                      left_bar = NULL,
                                      right_bar = NULL,
                                      top_bar = NULL,
                                      bottom_bar = NULL,
                                      left_hm = NULL,
                                      right_hm = NULL,
                                      top_hm = NULL,
                                      bottom_hm = NULL,
                                      show_legend = TRUE,
                                      annotation_colours = NULL) {
    ###########################################################################
    # Make sure dataframe is actually a dataframe and not a tibble etc.
    df <- data.frame(df)
    ###########################################################################
    # Ensure all the features specified are in the provided data
    check_colnames <- function(annotation_list, sorted_df) {
        if (!all(annotation_list %in% colnames(sorted_df))) {
            stop(
                "At least one feature specified for annotation is not",
                " present in the provided data_list."
            )
        }
    }
    ###########################################################################
    # Ensure every feature specified is given a name for plotting/legend
    check_listnames <- function(list) {
        if (length(list) != sum(names(list) != "", na.rm = TRUE)) {
            stop(
                "All features provided must in the annotation lists must be",
                " named."
            )
        }
    }
    ###########################################################################
    # Initialize the possible annotations
    left_annotations <- NULL
    right_annotations <- NULL
    top_annotations <- NULL
    bottom_annotations <- NULL
    ###########################################################################
    # Top barplots
    ###########################################################################
    if (!is.null(top_bar)) {
        check_colnames(top_bar, df)
        check_listnames(top_bar)
        top_bar_names <- names(top_bar)
        #######################################################################
        # Assign names to all the features in the top_bar
        #######################################################################
        for (i in seq_along(top_bar)) {
            ith_annotation <- ComplexHeatmap::HeatmapAnnotation(
                temporary_name = ComplexHeatmap::anno_barplot(
                    df[, top_bar[[i]]]
                ),
                show_legend = show_legend
            )
            # Remove the "temporary_name"s
            names(ith_annotation@anno_list) <- top_bar_names[[i]]
            ith_annotation@anno_list[[1]]@name <-
                top_bar_names[[i]]
            ith_annotation@anno_list[[1]]@label <-
                top_bar_names[[i]]
            ith_annotation@anno_list[[1]]@name_param$"label" <-
                top_bar_names[[i]]
            if (length(top_annotations) == 0) {
                top_annotations <- ith_annotation
            } else {
                top_annotations <- c(top_annotations, ith_annotation)
            }
        }
    }
    ###########################################################################
    # Top heatmaps
    ###########################################################################
    if (!is.null(top_hm)) {
        check_colnames(top_hm, df)
        check_listnames(top_hm)
        top_hm_names <- names(top_hm)
        #######################################################################
        # Assign names to all the features in the top_hm
        #######################################################################
        for (i in seq_along(top_hm)) {
            if (nchar(top_hm_names[[i]]) == 0) {
                top_hm_names[[i]] <- top_hm[[i]]
            }
            ith_annotation <- ComplexHeatmap::HeatmapAnnotation(
                temporary_name = df[, top_hm[[i]]],
                show_legend = show_legend
            )
            ###################################################################
            # Check for colours
            colour_position <- which(
                names(annotation_colours) == top_hm_names[[i]]
            )
            if (sum(colour_position) != 0) {
                ith_colour_map <- annotation_colours[colour_position]
                names(ith_colour_map) <- "temporary_name"
                ith_annotation <- ComplexHeatmap::HeatmapAnnotation(
                    temporary_name = df[, top_hm[[i]]],
                    col = ith_colour_map,
                    show_legend = show_legend
                )
            } else {
                ith_annotation <- ComplexHeatmap::HeatmapAnnotation(
                    temporary_name = df[, top_hm[[i]]],
                    show_legend = show_legend
                )
            }
            ###################################################################
            # Remove the "temporary_name"s
            names(ith_annotation@anno_list) <- top_hm_names[[i]]
            ith_annotation@anno_list[[1]]@name <-
                top_hm_names[[i]]
            ith_annotation@anno_list[[1]]@label <-
                top_hm_names[[i]]
            ith_annotation@anno_list[[1]]@color_mapping@name <-
                top_hm_names[[i]]
            ith_annotation@anno_list[[1]]@name_param$"label" <-
                top_hm_names[[i]]
            if (length(top_annotations) == 0) {
                top_annotations <- ith_annotation
            } else {
                top_annotations <- c(top_annotations, ith_annotation)
            }
        }
    }
    ###########################################################################
    # Bottom barplots
    ###########################################################################
    if (!is.null(bottom_bar)) {
        check_colnames(bottom_bar, df)
        check_listnames(bottom_bar)
        bottom_bar_names <- names(bottom_bar)
        #######################################################################
        # Assign names to all the features in the bottom_bar
        #######################################################################
        for (i in seq_along(bottom_bar)) {
            ith_annotation <- ComplexHeatmap::HeatmapAnnotation(
                temporary_name = ComplexHeatmap::anno_barplot(
                    df[, bottom_bar[[i]]]
                ),
                show_legend = show_legend
            )
            # Remove the "temporary_name"s
            names(ith_annotation@anno_list) <- bottom_bar_names[[i]]
            ith_annotation@anno_list[[1]]@name <-
                bottom_bar_names[[i]]
            ith_annotation@anno_list[[1]]@label <-
                bottom_bar_names[[i]]
            ith_annotation@anno_list[[1]]@name_param$"label" <-
                bottom_bar_names[[i]]
            if (length(bottom_annotations) == 0) {
                bottom_annotations <- ith_annotation
            } else {
                bottom_annotations <- c(bottom_annotations, ith_annotation)
            }
        }
    }
    ###########################################################################
    # Bottom heatmaps
    ###########################################################################
    if (!is.null(bottom_hm)) {
        check_colnames(bottom_hm, df)
        check_listnames(bottom_hm)
        bottom_hm_names <- names(bottom_hm)
        #######################################################################
        # Assign names to all the features in the bottom_hm
        #######################################################################
        for (i in seq_along(bottom_hm)) {
            if (nchar(bottom_hm_names[[i]]) == 0) {
                bottom_hm_names[[i]] <- bottom_hm[[i]]
            }
            ith_annotation <- ComplexHeatmap::HeatmapAnnotation(
                temporary_name = df[, bottom_hm[[i]]],
                show_legend = show_legend
            )
            ###################################################################
            # Check for colours
            colour_position <- which(
                names(annotation_colours) == bottom_hm_names[[i]]
            )
            if (sum(colour_position) != 0) {
                ith_colour_map <- annotation_colours[colour_position]
                names(ith_colour_map) <- "temporary_name"
                ith_annotation <- ComplexHeatmap::HeatmapAnnotation(
                    temporary_name = df[, bottom_hm[[i]]],
                    col = ith_colour_map,
                    show_legend = show_legend
                )
            } else {
                ith_annotation <- ComplexHeatmap::HeatmapAnnotation(
                    temporary_name = df[, bottom_hm[[i]]],
                    show_legend = show_legend
                )
            }
            ###################################################################
            # Remove the "temporary_name"s
            names(ith_annotation@anno_list) <- bottom_hm_names[[i]]
            ith_annotation@anno_list[[1]]@name <-
                bottom_hm_names[[i]]
            ith_annotation@anno_list[[1]]@label <-
                bottom_hm_names[[i]]
            ith_annotation@anno_list[[1]]@color_mapping@name <-
                bottom_hm_names[[i]]
            ith_annotation@anno_list[[1]]@name_param$"label" <-
                bottom_hm_names[[i]]
            if (length(bottom_annotations) == 0) {
                bottom_annotations <- ith_annotation
            } else {
                bottom_annotations <- c(bottom_annotations, ith_annotation)
            }
        }
    }
    ###########################################################################
    # Left barplots
    ###########################################################################
    if (!is.null(left_bar)) {
        check_colnames(left_bar, df)
        check_listnames(left_bar)
        left_bar_names <- names(left_bar)
        #######################################################################
        # Assign names to all the features in the left_bar
        #######################################################################
        for (i in seq_along(left_bar)) {
            ith_annotation <- ComplexHeatmap::rowAnnotation(
                temporary_name = ComplexHeatmap::anno_barplot(
                    df[, left_bar[[i]]]
                ),
                show_legend = show_legend
            )
            # Remove the "temporary_name"s
            names(ith_annotation@anno_list) <- left_bar_names[[i]]
            ith_annotation@anno_list[[1]]@name <-
                left_bar_names[[i]]
            ith_annotation@anno_list[[1]]@label <-
                left_bar_names[[i]]
            ith_annotation@anno_list[[1]]@name_param$"label" <-
                left_bar_names[[i]]
            if (length(left_annotations) == 0) {
                left_annotations <- ith_annotation
            } else {
                left_annotations <- c(left_annotations, ith_annotation)
            }
        }
    }
    ###########################################################################
    # Left heatmaps
    ###########################################################################
    if (!is.null(left_hm)) {
        check_colnames(left_hm, df)
        check_listnames(left_hm)
        left_hm_names <- names(left_hm)
        #######################################################################
        # Assign names to all the features in the left_hm
        #######################################################################
        for (i in seq_along(left_hm)) {
            if (nchar(left_hm_names[[i]]) == 0) {
                left_hm_names[[i]] <- left_hm[[i]]
            }
            ###################################################################
            # Check for colours
            colour_position <- which(
                names(annotation_colours) == left_hm_names[[i]]
            )
            if (sum(colour_position) != 0) {
                ith_colour_map <- annotation_colours[colour_position]
                names(ith_colour_map) <- "temporary_name"
                ith_annotation <- ComplexHeatmap::rowAnnotation(
                    temporary_name = df[, left_hm[[i]]],
                    col = ith_colour_map,
                    show_legend = show_legend
                )
            } else {
                ith_annotation <- ComplexHeatmap::rowAnnotation(
                    temporary_name = df[, left_hm[[i]]],
                    show_legend = show_legend
                )
            }
            ###################################################################
            # Remove the "temporary_name"s
            names(ith_annotation@anno_list) <- left_hm_names[[i]]
            ith_annotation@anno_list[[1]]@name <-
                left_hm_names[[i]]
            ith_annotation@anno_list[[1]]@label <-
                left_hm_names[[i]]
            ith_annotation@anno_list[[1]]@color_mapping@name <-
                left_hm_names[[i]]
            ith_annotation@anno_list[[1]]@name_param$"label" <-
                left_hm_names[[i]]
            if (length(left_annotations) == 0) {
                left_annotations <- ith_annotation
            } else {
                left_annotations <- c(left_annotations, ith_annotation)
            }
        }
    }
    ###########################################################################
    # Right barplots
    ###########################################################################
    if (!is.null(right_bar)) {
        check_colnames(right_bar, df)
        check_listnames(right_bar)
        right_bar_names <- names(right_bar)
        #######################################################################
        # Assign names to all the features in the right_bar
        #######################################################################
        for (i in seq_along(right_bar)) {
            ith_annotation <- ComplexHeatmap::rowAnnotation(
                temporary_name = ComplexHeatmap::anno_barplot(
                    df[, right_bar[[i]]]
                ),
                show_legend = show_legend
            )
            # Remove the "temporary_name"s
            names(ith_annotation@anno_list) <- right_bar_names[[i]]
            ith_annotation@anno_list[[1]]@name <-
                right_bar_names[[i]]
            ith_annotation@anno_list[[1]]@label <-
                right_bar_names[[i]]
            ith_annotation@anno_list[[1]]@name_param$"label" <-
                right_bar_names[[i]]
            if (length(right_annotations) == 0) {
                right_annotations <- ith_annotation
            } else {
                right_annotations <- c(right_annotations, ith_annotation)
            }
        }
    }
    ###########################################################################
    # Right heatmaps
    ###########################################################################
    if (!is.null(right_hm)) {
        check_colnames(right_hm, df)
        check_listnames(right_hm)
        right_hm_names <- names(right_hm)
        #######################################################################
        # Assign names to all the features in the right_hm
        #######################################################################
        for (i in seq_along(right_hm)) {
            if (nchar(right_hm_names[[i]]) == 0) {
                right_hm_names[[i]] <- right_hm[[i]]
            }
            ith_annotation <- ComplexHeatmap::rowAnnotation(
                temporary_name = df[, right_hm[[i]]],
                show_legend = show_legend
            )
            ###################################################################
            # Check for colours
            colour_position <- which(
                names(annotation_colours) == right_hm_names[[i]]
            )
            if (sum(colour_position) != 0) {
                ith_colour_map <- annotation_colours[colour_position]
                names(ith_colour_map) <- "temporary_name"
                ith_annotation <- ComplexHeatmap::rowAnnotation(
                    temporary_name = df[, right_hm[[i]]],
                    col = ith_colour_map,
                    show_legend = show_legend
                )
            } else {
                ith_annotation <- ComplexHeatmap::rowAnnotation(
                    temporary_name = df[, right_hm[[i]]],
                    show_legend = show_legend
                )
            }
            ###################################################################
            # Remove the "temporary_name"s
            names(ith_annotation@anno_list) <- right_hm_names[[i]]
            ith_annotation@anno_list[[1]]@name <-
                right_hm_names[[i]]
            ith_annotation@anno_list[[1]]@label <-
                right_hm_names[[i]]
            ith_annotation@anno_list[[1]]@color_mapping@name <-
                right_hm_names[[i]]
            ith_annotation@anno_list[[1]]@name_param$"label" <-
                right_hm_names[[i]]
            if (length(right_annotations) == 0) {
                right_annotations <- ith_annotation
            } else {
                right_annotations <- c(right_annotations, ith_annotation)
            }
        }
    }
    ###########################################################################
    # Return final output
    annotations_list <- list(
        left_annotations = left_annotations,
        right_annotations = right_annotations,
        top_annotations = top_annotations,
        bottom_annotations = bottom_annotations
    )
    return(annotations_list)
}

#' Convert a vector of partition indices into meta cluster labels
#'
#' @param split_vector A vector of partition indices.
#'
#' @param nrow The number of rows in the data being partitioned.
#'
#' @return A character vector that expands the split_vector into an nrow-length
#' sequence of ascending letters of the alphabet. If the split vector is
#' c(3, 6) and the number of rows is 8, the result will be a vector of two
#' "A"s (up to the first index, 3), three "B"s (up to the second index, 6),
#' and three "C"s (up to and including the last index, 8).
#'
#' @export
label_splits <- function(split_vector, nrow) {
    labels <- rep("A", nrow)
    if (split_vector[length(split_vector)] != nrow) {
        split_vector <- c(split_vector, nrow)
    }
    for (i in 1:(length(split_vector) - 1)) {
        start <- split_vector[i]
        end <- split_vector[i + 1]
        labels[start:end] <- LETTERS[i + 1]
    }
    return(labels)
}

#' Save a heatmap object to a file
#'
#' @param heatmap The heatmap object to save.
#'
#' @param path The path to save the heatmap to.
#'
#' @param width The width of the heatmap.
#'
#' @param height The height of the heatmap.
#'
#' @param res The resolution of the heatmap.
#'
#' @return Does not return any value. Saves heatmap to file.
#'
#' @export
save_heatmap <- function(heatmap,
                         path,
                         width = 480,
                         height = 480,
                         res = 100) {
    grDevices::png(
        filename = path,
        width = width,
        height = height,
        res = res
    )
    ComplexHeatmap::draw(heatmap)
    grDevices::dev.off()
}

#' Return the hierarchical clustering order of a matrix
#'
#' @param matrix Matrix to cluster.
#'
#' @param dist_method Distance method to apply to the matrix. Argument is
#' directly passed into stats::dist. Options include "euclidean", "maximum",
#' "manhattan", "canberra", "binary", or "minkowski".
#'
#' @param hclust_method Which agglomerative method to be passed into
#' stats::hclust. Options include "ward.D", "ward.D2", "single", "complete",
#' "average", "mcquitty", "median", or "centroid".
#'
#' @return A numeric vector of the ordering derivied by the specified
#' hierarchical clustering method applied to the provided matrix.
#'
#' @export
get_matrix_order <- function(matrix,
                             dist_method = "euclidean",
                             hclust_method = "complete") {
    distance_matrix <- stats::dist(matrix, method = dist_method)
    hclust_result <- stats::hclust(distance_matrix, method = hclust_method)
    order <- hclust_result$order
    return(order)
}

#' Return the row or column ordering present in a heatmap
#'
#' @param heatmap A heatmap object to collect ordering from.
#'
#' @param type The type of ordering to return. Either "rows" or "columns".
#'
#' @return A numeric vector of the ordering used within the provided
#' ComplexHeatmap "Heatmap" object.
#'
#' @export
get_heatmap_order <- function(heatmap, type = "rows") {
    drawn_heatmap <- ComplexHeatmap::draw(heatmap)
    if (type == "rows") {
        order <- ComplexHeatmap::row_order(drawn_heatmap)
    } else if (type == "columns") {
        order <- ComplexHeatmap::column_order(drawn_heatmap)
    } else {
        stop("Valid types are 'rows' and 'columns'.")
    }
    return(order)
}

#' Helper function to determine which row and columns to split on
#'
#' @param row_split_vector A vector of row indices to split on.
#'
#' @param column_split_vector A vector of column indices to split on.
#'
#' @param row_split Standard parameter of `ComplexHeatmap::Heatmap`.
#'
#' @param column_split Standard parameter of `ComplexHeatmap::Heatmap`.
#'
#' @param n_rows The number of rows in the data.
#'
#' @param n_columns The number of columns in the data.
#'
#' @return "list"-class object containing row_split and column_split character
#' vectors to pass into ComplexHeatmap::Heatmap.
#'
#' @export
split_parser <- function(row_split_vector = NULL,
                         column_split_vector = NULL,
                         row_split = NULL,
                         column_split = NULL,
                         n_rows,
                         n_columns) {
    if (is.null(column_split) + is.null(column_split_vector) == 0) {
        warning(
            "column_split and column_split_vector arguments were both",
            " provided. Only column_split_vector will be used to determine",
            " plot gaps."
        )
    }
    if (is.null(column_split) + is.null(column_split_vector) == 0) {
        warning(
            "row_split and row_split_vector arguments were both",
            " provided. Only row_split_vector will be used to determine",
            " plot gaps."
        )
    }
    if (!is.null(row_split_vector)) {
        row_split <- label_splits(row_split_vector, n_rows)
    }
    if (!is.null(column_split_vector)) {
        column_split <- label_splits(column_split_vector, n_columns)
    }
    split_results <- list(
        "row_split" = row_split,
        "column_split" = column_split
    )
    return(split_results)
}

#' Helper function to stop annotation building when no data was provided
#'
#' @param data A dataframe with data to build annotations
#' @param annotation_requests A list of requested annotations
#'
#' @return Does not return any value. This function just raises an error when
#' annotations are requested without any provided data for a heatmap.
#'
#' @export
check_dataless_annotations <- function(annotation_requests, data) {
    any_null_annotations <- lapply(annotation_requests, is.null) |>
        unlist() |>
        any()
    if (!any_null_annotations) {
        if (is.null(data)) {
            stop(
                "You must provide data, either through a data_list or a",
                " dataframe passed in with the 'data' parameter to use",
                " annotations."
            )
        }
    }
}

#' Check for ComplexHeatmap and circlize dependencies
#'
#' @return Does not return any value. This function just checks that the
#' ComplexHeatmap and circlize packages are installed.
#'
#' @export
check_hm_dependencies <- function() {
    if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
        stop(
            "Package \"ComplexHeatmap\" is required to use this function.",
            " The package is available on BioConductor:",
            " https://bioconductor.org/packages/",
            "release/bioc/html/ComplexHeatmap.html",
            call. = FALSE
        )
    }
    if (!requireNamespace("circlize", quietly = TRUE)) {
        stop(
            "Package \"circlize\" is required to use this function.",
            " The package is available on CRAN:",
            " `install.packages(\"circlize\")`.",
            call. = FALSE
        )
    }
}


