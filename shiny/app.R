library(shiny)
library(metasnf)
library(InteractiveComplexHeatmap)
library(ComplexHeatmap)

shiny_env <- new.env()
shiny_env$i_widget <- 0
shiny_env$i_obs <- 0
shiny_env$heatmap <- list()
shiny_env$obs <- list()
shiny_env$action_button_count <- list()
shiny_env$initialized <- list()

###############################################################################
client_ui <- function(heatmap_id = NULL,
                      title1 = "Clustered Adjusted Rand Indices",
                      title3 = NULL,
                      width1 = 600,
                      height1 = 600,
                      layout = "1-3",
                      compact = FALSE,
                      action = "click",
                      cursor = TRUE,
                      response = c(action, "brush"),
                      brush_opt = list(stroke = "#f00", opacity = 0.6),
                      output_ui = htmlOutput(paste0(heatmap_id, "_info")),
                      output_ui_float = FALSE,
                      containment = FALSE,
                      internal = FALSE,
                      ...) {

    if (is.null(heatmap_id)) {
        shiny_env$i_widget <- shiny_env$i_widget + 1
        heatmap_id <- paste0("ht", shiny_env$i_widget)
    }

    main_heatmap_ui <- originalHeatmapOutput(
        heatmap_id,
        title = title1,
        width = width1,
        height = height1,
        action = action,
        cursor = cursor,
        response = response,
        brush_opt = brush_opt,
        containment = containment,
        internal = internal
    )

    output_ui <- HeatmapInfoOutput(
        heatmap_id,
        title = title3,
        width = 400,
        output_ui = output_ui,
        output_ui_float = output_ui_float,
        action = action,
        response = response,
        internal = internal
    )

    layout_css <- GetoptLong::qq("
        .@{heatmap_id}_widget #@{heatmap_id}_heatmap_group {
            display:table-cell;
        }
        .@{heatmap_id}_widget #@{heatmap_id}_output_wrapper {
            display:table-cell;
        }
    ")

    tl <- tagList(
        main_heatmap_ui,
        output_ui
    )

    fluidPage(class = GetoptLong::qq("@{heatmap_id}_widget"),
        tl,
        tags$style(HTML(layout_css)),
        ...
    )
}

ht_shiny <- function(ht_list = ComplexHeatmap:::.ENV$last,
                     title = NULL,
                     description = NULL,
                     hline = TRUE,
                     html = NULL,
                     heatmap_id = NULL,
                     title1 = "Original heatmap",
                     width1 = 700, # The width of the heatmap
                     height1 = 700, # The height of the heatmap
                     layout = "1-3",
                     compact = FALSE,
                     action = "click",
                     cursor = TRUE,
                     response = c(action, "brush"),
                     brush_opt = list(stroke = "#f00", opacity = 0.6),
                     output_ui_float = FALSE,
                     app_options = list()) {
    title <- titlePanel(title)
    description <- p(description)

    ui <- fluidPage(
        title,
        description,
        if (hline) hr() else NULL,
        client_ui(
            heatmap_id = heatmap_id,
            title1 = title1,
            width1 = width1,
            height1 = height1,
            layout = layout,
            compact = compact,
            action = action,
            cursor = cursor,
            response = response,
            brush_opt = brush_opt,
            output_ui_float = output_ui_float
        ),
        html
    )

    server <- function(input, output, session) {
        makeInteractiveComplexHeatmap(
            input,
            output,
            session,
            ht_list
        )
    }

    shinyApp(ui, server, options = app_options)
}

###############################################################################

data_list <- data_list(
    list(
        data = abcd_cort_t,
        name = "cortical_thickness",
        domain = "neuroimaging",
        type = "continuous"
    ),
    list(
        data = abcd_cort_sa,
        name = "cortical_surface_area",
        domain = "neuroimaging",
        type = "continuous"
    ),
    list(
        data = abcd_subc_v,
        name = "subcortical_volume",
        domain = "neuroimaging",
        type = "continuous"
    ),
    list(
        data = abcd_h_income,
        name = "household_income",
        domain = "demographics",
        type = "continuous"
    ),
    list(
        data = abcd_pubertal,
        name = "pubertal_status",
        domain = "demographics",
        type = "continuous"
    ),
    uid = "patient"
)

settings_matrix <- generate_settings_matrix(
    data_list,
    nrow = 20,
    min_k = 20,
    max_k = 50,
    seed = 42
)

solutions_matrix <- batch_snf(data_list, settings_matrix)

solutions_matrix_aris <- calc_aris(solutions_matrix)

meta_cluster_order <- get_matrix_order(solutions_matrix_aris)

ari_hm <- adjusted_rand_index_heatmap(
    solutions_matrix_aris,
    order = meta_cluster_order
)

#' Launch shiny app to identify meta cluster boundaries
#'
#' @param ari_heatmap Heatmap of ARIs to divide into meta clusters.
#'
#' @export
shiny_annotator <- function(ari_heatmap) {
    drawn_heatmap <- ComplexHeatmap::draw(ari_heatmap)
    ht_shiny(
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
            " `split_vector = c(150, 300, 313) when recreating the heatmap.",
            " This app is based on the `htShiny` function from the",
            " `InteractiveComplexHeatmap` package (Gu, Z. (2021)):",
            " https://jokergoo.github.io/InteractiveComplexHeatmap/index.html"
        )
    )
}

shiny_annotator(ari_hm)
