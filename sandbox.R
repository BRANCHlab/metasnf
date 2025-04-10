#' '# below may help for extending filter
#' '#' @export
#' '#' @importFrom dplyr filter
#' 'filter.solutions_df <- function(.data, ...) {
#' '    NextMethod()
#' '}
#' '
#' '#' @export
#' '#'
#' 'dplyr::filter

devtools::load_all()

library(metasnf)

help(package = "metasnf")


# `data_list` class object stores data frames and metadata
dl <- data_list(
    list(cort_sa, "cortical_sa", "neuroimaging", "continuous"),
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)

set.seed(42)
config <- snf_config(
    dl,
    n_solutions = 20,
    dropout_dist = "none",
    max_k = 40
)

sol_df <- batch_snf(dl, config)


circlize::colorRamp2(
    c(0.001, 0.01, 0.05, 1),
    c("red2", "purple", "lightblue", "grey")
)

ari_mat <- calc_aris(sol_df)
sol_mat <- sol_df |>
    as.data.frame() |>
    dplyr::select(dplyr::starts_with("uid")) |>
    as.matrix()

plot(sol_df, cluster_rows = TRUE)

vignettes <- c(
    "a_complete_example.Rmd", # 4.95
    "correlation_plots.Rmd", # 1.13
    "getting_started.Rmd", #0.3
    "parallel_processing.Rmd", #0.31
    "stability_measures.Rmd", #0.91
    "alluvial_plots.Rmd", #1.78
    "data_list.Rmd", #0.38
    "imputations.Rmd", #0.31
    "quality_measures.Rmd", # 0.3
    "troubleshooting.Rmd", # 0.08
    "a_simple_example.Rmd", # 2.3
    "distance_metrics.Rmd", # 0.52
    "label_propagation.Rmd", # 25.9
    "similarity_matrix_heatmap.Rmd", # 1.32
    "clustering_algorithms.Rmd", # 5.53
    "feature_plots.Rmd", # 2.55
    "manhattan_plots.Rmd", # 1.12
    "snf_config.Rmd", # 1.81
    "confounders.Rmd", # 1.83
    "feature_weights.Rmd",  # 0.47
    "nmi_scores.Rmd", # 2.08
    "snf_schemes.Rmd" # 0.08
)
df <- data.frame()
for (v in vignettes) {
    cat("Rendering vignette: ", v, "\n")
    start <- Sys.time()
    rmarkdown::render(v)
    time <- Sys.time() - start
    df <- rbind(
        df,
        data.frame(
            vignette = v,
            time = time
        )
    )
}
df |>
    dplyr::arrange(-time)

getwd()

cache_a_complete_example_sol_df

cache_a_complete_example_lp_ext_sol_df

system.time( rmarkdown::render("vignettes/my-vignette.Rmd"))
