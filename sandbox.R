# Note: when testing optional packages, see https://r-pkgs.org/dependencies-in-practice.html#sec-dependencies-in-suggests-in-tests

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

batch_snf_results <- batch_snf(
    data_list,
    settings_matrix,
    return_affinity_matrices = TRUE
)

solutions_matrix <- batch_snf_results$"solutions_matrix"
affinity_matrices <- batch_snf_results$"affinity_matrices"

data_list_subsamples <- subsample_data_list(
    data_list,
    n_subsamples = 3,
    subsample_fraction = 0.8
)



subsample_pairwise_aris(data_list_subsamples, settings_matrix)

fraction_clustered_together(data_list_subsamples, settings_matrix, solutions_matrix)

solution_index



# library(dbscan)

# Stability:
# - adjusted rand index across all iterations X


start <- proc.time()


proc.time() - start

solutions_matrix
