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

dl <- data_list(
    list(income, "household_income", "demographics", "ordinal"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    list(fav_colour, "favourite_colour", "demographics", "categorical"),
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    list(depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)

sc <- snf_config(
    dl,
    n_solutions = 10,
    dropout_dist = "uniform"
)

plot(sc)

summary(mock_snf_config$"dist_fns_list")

plot(mock_settings_df)

splot(mock_snf_config)

plot(mock_weights_matrix)

plot(mock_snf_config)

summary(mock_solutions_df)

summary(t(mock_ext_solutions_df))

(mock_weights_matrix)

mock_aris

plot(mock_ari_matrix)

meta_cluster_heatmap(mock_ari_matrix)
