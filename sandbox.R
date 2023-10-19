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


solutions_matrix

# library(dbscan)

distance_metrics_list[[1]]

distance_metrics_list <- generate_distance_metrics_list(
    continuous_distances = list(
        "my_dist" = euclidean_distance,
        "3" = euclidean_distance
    ),
    discrete_distances = list(
        "my_dist2" = euclidean_distance,
        "my_dist3" = euclidean_distance
    )
)


settings_matrix <- generate_settings_matrix(
    data_list,
    nrow = 5,
    max_k = 40,
    seed = 42,
    distance_metrics_list = distance_metrics_list
)

settings_matrix

distance_metrics_list <- generate_distance_metrics_list()

summarize_distance_metrics_list(distance_metrics_list)

# Using just the base distance metrics  ------------------------------------
distance_metrics_list <- generate_distance_metrics_list()

# Adding your own metrics --------------------------------------------------
# This will contain the base and user-provided clustering algorithms
my_distance_metric <- function(df) {
    # your code that converts a dataframe to a distance metric here...
    # return(distance_metric)
}

distance_metrics_list <- generate_distance_metrics_list(
    continuous_distances = list(
         "my_distance_metric" = my_distance_metric
    )
)

# Suppress the base metrics-------------------------------------------------
# This will contain only user-provided clustering algorithms

distance_metrics_list <- generate_distance_metrics_list(
    continuous_distances = list(
         "my_distance_metric" = my_distance_metric
    ),
    keep = TRUE
)
