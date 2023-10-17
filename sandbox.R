library(metasnf)

data_list <- generate_data_list(
    list(abcd_cort_t, "cortical_thickness", "neuroimaging", "numeric"),
    list(abcd_cort_sa, "cortical_surface_area", "neuroimaging", "numeric"),
    list(abcd_subc_v, "subcortical_volume", "neuroimaging", "numeric"),
    list(abcd_income, "household_income", "demographics", "numeric"),
    list(abcd_pubertal, "pubertal_status", "demographics", "numeric"),
    old_uid = "patient"
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

solutions_matrix2 <- batch_snf(
    data_list,
    settings_matrix
)

# library(dbscan)

data <- data_list[[1]]$"data"

data <- data.frame(data, row.names = 1)

row.names(data) <- NULL

data

as.matrix(stats::dist(data, method = "euclidean"))

euclidean_distance <- function(df) {
    # Remove the first column, which is just the subjectkey
    df <- df[, -1]
    # Apply euclidean distance
    distance_matrix <- df |>
        stats::dist(method = "euclidean") |>
        as.matrix()
    return(distance_matrix)
}

gower_distance <- function(df) {
    # Remove the first column, which is just the subjectkey
    df <- df[, -1]
    # Convert all character columns into factors
    df <- char_to_fac(df)
    distance_matrix <- df |>
        cluster::daisy(metric = "gower", warnBin = FALSE) |>
        as.matrix()
}



generate_distance_metrics_list(
    continuous_distances = list(
        "cat" = 5,
        3
    )
)

library(SNFtool)

SNF
