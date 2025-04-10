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
