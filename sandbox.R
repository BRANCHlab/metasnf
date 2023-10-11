library(metasnf)

data_list <- generate_data_list(
    list(abcd_cort_t, "cortical_thickness", "neuroimaging", "numeric"),
    list(abcd_cort_sa, "cortical_surface_area", "neuroimaging", "numeric"),
    list(abcd_subc_v, "subcortical_volume", "neuroimaging", "numeric"),
    list(abcd_income, "household_income", "demographics", "numeric"),
    list(abcd_pubertal, "pubertal_status", "demographics", "numeric"),
    old_uid = "patient"
)

settings_matrix <- generate_settings_matrix(data_list, nrow = 15, seed = 42)

# Should save many affinity matrices to disk
# output_matrix <- batch_snf(data_list, settings_matrix, affinity_matrix_dir = ".")

# Should error
# output_matrix <- batch_snf(data_list, settings_matrix, run_clustering = FALSE)

# Should eventually complete
# output_matrix <- batch_snf(data_list, settings_matrix, processes = "max")

# Normal
output_matrix <- batch_snf(data_list, settings_matrix)


clust_algs_list <- generate_clust_algs_list(
    "banana_alg" = spectral_rot,
    "spectral_banana" = spectral_rot
)

clust_algs_list <- generate_clust_algs_list()

names(clust_algs_list)

summarize_clust_algs_list(clust_algs_list)


colnames(settings_matrix)



a <- list(
    "cheese" = 1,
    "cheeser" = 2
)

b <- list(
    3,
    4
)

q <- c(a, b)

