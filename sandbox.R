# Load the package
library(metasnf)

# Setting up the data
data_list <- generate_data_list(
    list(abcd_income, "household_income", "demographics", "continuous"),
    list(abcd_pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "patient"
)

# Specifying 5 different sets of settings for SNF
settings_matrix <- generate_settings_matrix(
    data_list,
    nrow = 3,
    max_k = 40
)

solutions_matrix <- batch_snf(data_list, settings_matrix)

solutions_matrix_parallel <- batch_snf(
    data_list,
    settings_matrix,
    processes = 2
)

all.equal(solutions_matrix, solutions_matrix_parallel)
