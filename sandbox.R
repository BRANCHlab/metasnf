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

output_matrix <- batch_snf(data_list, settings_matrix, affinity_matrix_dir = ".")

write.csv(output_matrix, "mymatrix.csv", row.names = TRUE)
om_read <- read.csv("mymatrix.csv", row.names = 1)

i <- 10
affinity_matrix_dir <- "this/is/the-path"



