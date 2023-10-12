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
    possible_k = seq(10, 100, by = 10),
    possible_snf_schemes = c(1, 2)
)

sample(20:30, 1)

settings_matrix$"t"

settings_matrix$alpha

settings_matrix$k

colnames(settings_matrix)

settings_matrix$"t"

# Should save many affinity matrices to disk
# output_matrix <- batch_snf(data_list, settings_matrix, affinity_matrix_dir = ".")

# Should error
# output_matrix <- batch_snf(data_list, settings_matrix, run_clustering = FALSE)

# Should eventually complete
# output_matrix <- batch_snf(data_list, settings_matrix, processes = "max")

# Normal
output_matrix <- batch_snf(data_list, settings_matrix)

output_matrix

sample.int(1:3, 3, 1)

resample <- function(x, ...) {
    print(length(x))
    x[sample.int(length(x), ...)]
}

resample(1:3, 1)

resample(3, 1)

[sample.int(10, 1)]


