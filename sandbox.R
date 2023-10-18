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



distance_metrics_list[[1]]

distance_metrics_list <- generate_distance_metrics_list()

distance_metrics_list

summarize_distance_metrics_list(distance_metrics_list)

# in the settings matrix, either accept user specified values or
# simply base it on the number of values in generate_distance_metrics_list


