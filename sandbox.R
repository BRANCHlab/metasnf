# Load the package
library(metasnf)

# Setting up the data
data_list <- generate_data_list(
    list(abcd_cort_t, "cortical_thickness", "neuroimaging", "continuous"),
    list(abcd_cort_sa, "cortical_surface_area", "neuroimaging", "continuous"),
    list(abcd_subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(abcd_income, "household_income", "demographics", "continuous"),
    list(abcd_pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "patient"
)

# Specifying 5 different sets of settings for SNF
settings_matrix <- generate_settings_matrix(
    data_list,
    nrow = 5,
    max_k = 40,
    seed = 42
)

# This matrix has clustering solutions for each of the 5 SNF runs!
solutions_matrix <- batch_snf(data_list, settings_matrix)

target_list <- generate_target_list(
    list(abcd_anxiety, "anxiety", "ordinal"),
    list(abcd_depress, "depressed", "numeric"),
    list(abcd_colour, "colour", "categorical"),
    uid = "patient"
)

extended_solutions <- extend_solutions(
    solutions_matrix,
    target_list
)

extended_solutions <- extend_solutions(
    solutions_matrix,
    target_list,
    cat_test = "fisher_exact"
)
