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


pairwise_ari_df <- subsample_pairwise_aris(data_list_subsamples, settings_matrix)


pairwise_ari_df

# solutions matrix row
# mean ari
# ari sd

# library(dbscan)

# Stability:
# - percentage of time that a patient clustered with another patient from their own cluster
# - adjusted rand index across all iterations
