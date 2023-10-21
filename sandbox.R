library(metasnf)

data_list <- generate_data_list(
    list(abcd_cort_t, "cortical_thickness", "neuroimaging", "numeric"),
    list(abcd_cort_sa, "cortical_surface_area", "neuroimaging", "numeric"),
    list(abcd_subc_v, "subcortical_volume", "neuroimaging", "numeric"),
    list(abcd_income, "household_income", "demographics", "numeric"),
    list(abcd_pubertal, "pubertal_status", "demographics", "numeric"),
    uid = "patient"
)

settings_matrix <- generate_settings_matrix(
    data_list,
    nrow = 5,
    max_k = 40,
    seed = 42
)

solutions_matrix <- batch_snf(
    data_list,
    settings_matrix
)

weighted_euclidean_distance

mock_data <- data.frame(
    col1 = c(1, 4, 7),
    col2 = c(1, 5, 9)
) |> as.matrix()

mock_data

weighted_euclidean_distance(mock_data, weights_df)

weighted_euclidean_distance(mock_data, data.frame(c(1, 1)))

weights_df <- data.frame(
    weights = c(1, 2)
)

weights_df

mock_data

wt_sq_euclidean_distance <- function(df, weights) {
}


weights_matrix <- generate_weights_matrix(
    data = mock_data,
    rows = 10,
    fill = "uniform"
)

# May need a left or right join to properly reduce the weights df row



#' Distance metric: Weighted Euclidean distance
#'
#' @param df Dataframe containing one subjectkey column in the first column and
#'  at least 1 continuous data column. All feature data should be continuous.
#' @param weights Dataframe with 1 column containing weights for each feature per
#'  row in the same order as the order of feature columns start
#'
#' @return weighted_distance_matrix A distance matrix.
#'
#' @export
weighted_euclidean_distance <- function(df, weights) {
    if (!requireNamespace("abSNF", quietly = TRUE)) {
        stop(
            "Package \"abSNF\" must be installed to use this function.",
            call. = FALSE
        )
    }
    weights_mat <- data.matrix(weights)
    weighted_dist <- abSNF::dist2_w(
        X = df,
        C = df,
        weight = weights_mat
    )
    return(weighted_dist)
}
