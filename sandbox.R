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

# `data_list` class object stores data frames and metadata
dl <- data_list(
    list(methylation_df, "cortical_sa", "neuroimaging", "continuous"),
    list(expression_df, "cortical_t", "neuroimaging", "continuous"),
    uid = "patient_id"
)

# `data_list` class object stores data frames and metadata
dl <- data_list(
    list(income_df, "cortical_sa", "neuroimaging", "continuous"),
    list(, "cortical_t", "neuroimaging", "continuous"),
    uid = "unique_id"
)


help(package = "metasnf")
