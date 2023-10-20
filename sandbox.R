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

weights_matrix_base <- function(data = NULL,
                                data_list = NULL,
                                rows = 1,
                                fill = "ones") {
    if (is.null(data) + is.null(data_list) != 1) {
        stop(
            paste0(
                "One (and only one) of data or data_list parameter must be",
                "provided."
            )
        )
    }
    if (!is.null(data_list)) {
        matrix_colnames <- data_list |>
            lapply(
                function(x) {
                    colnames(x$"data")[colnames(x$"data") != "subjectkey"]
                }
            ) |>
            unlist()
    } else {
        matrix_colnames <- colnames(data)[colnames(data) != "subjectkey"]
    }
    if (fill == "ones") {
        fill <- 1
    } else if (fill == "uniform") {
        fill <- runif(rows * length(matrix_colnames))
    } else if (fill == "exponential") {
        fill <- rexp(rows * length(matrix_colnames))
    }
    matrix_base <- matrix(
        nrow = rows,
        ncol = length(matrix_colnames),
        data = fill
    )
    colnames(matrix_base) <- matrix_colnames
    matrix_base
}

weights_matrix <- generate_weights_matrix(
    data_list = data_list,
    rows = 10,
    fill = "uniform"
)



weights_matrix
