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


solutions_matrix <- batch_snf(
    data_list,
    settings_matrix,
    processes = "max"
)

solutions_matrix

data_list_d <- solutions_matrix$"data_list"
distance_metrics_list_d <- solutions_matrix$"distance_metrics_list"
clust_algs_list_d <- solutions_matrix$"clust_algs_list"
settings_matrix_d <- solutions_matrix$"settings_matrix"
weights_matrix_d <- solutions_matrix$"weights_matrix"
processes_d <- solutions_matrix$"processes"

weights_and_settings <- cbind(settings_matrix_d, weights_matrix_d)

batch_row_function <- batch_row_closure(
    data_list_d,
    distance_metrics_list_d,
    clust_algs_list_d,
    settings_matrix_d,
    weights_matrix_d
)
q <- apply(
    weights_and_settings,
    1,
    batch_row_function
)

sm <- do.call("rbind", q)


solutions_matrix <- batch_snf(data_list, settings_matrix)

sm <- sm[, colnames(solutions_matrix)]


future.apply::future_mapply(
    batch_row_function,
    settings_matrix_d,
    weights_matrix_d
)

colnames(weights_matrix_d)

colnames(settings_matrix_d)

print(length(settings_matrix_d))



print(length(weights_matrix_d))

# Check the lengths of vectors before using mapply
if(length(settings_matrix_d) == length(weights_matrix_d)) {
  result <- mapply(z, settings_matrix_d, weights_matrix_d)
  # Continue with further operations using 'result'
} else {
  # Perform appropriate actions if lengths are not compatible
  print("Lengths of vectors are not compatible for element-wise operations.")
}




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
