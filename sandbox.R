library(metasnf)

data_list <- generate_data_list(
    list(abcd_cort_t, "cortical_thickness", "neuroimaging", "numeric"),
    list(abcd_cort_sa, "cortical_surface_area", "neuroimaging", "numeric"),
    list(abcd_subc_v, "subcortical_volume", "neuroimaging", "numeric"),
    list(abcd_income, "household_income", "demographics", "numeric"),
    list(abcd_pubertal, "pubertal_status", "demographics", "numeric"),
    old_uid = "patient"
)

length(data_list)

settings_matrix <- generate_settings_matrix(data_list, nrow = 15, seed = 42)


settings_matrix <- generate_settings_matrix(
    data_list,
    nrow = 15,
    seed = 42,
    min_alpha = 0.3,
    max_alpha = 0.8,
    possible_snf_schemes = c(1, 2)
)

settings_matrix$alpha

settings_matrix

# Should save many affinity matrices to disk
# output_matrix <- batch_snf(data_list, settings_matrix, affinity_matrix_dir = ".")

# Should error
# output_matrix <- batch_snf(data_list, settings_matrix, run_clustering = FALSE)

# Should eventually complete
# output_matrix <- batch_snf(data_list, settings_matrix, processes = "max")

# Normal
output_matrix <- batch_snf(data_list, settings_matrix)

output_matrix


library(SNFtool)

affinityMatrix

min_alpha <- NULL
max_alpha <- NULL

if (min_alpha < 0.3 | max_alpha > 0.8) {
    warn(
        "Requested minimum / maximum alpha hyperparameter range is out of",
        " range empirically determined to be reasonable (0.3 to 0.8)."
    )
}
