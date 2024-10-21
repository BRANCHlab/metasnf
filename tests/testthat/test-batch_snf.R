#library(metasnf)
#library(testthat)

###############################################################################
# batch_snf()
###############################################################################
test_that("function and parallel equivalent run and give equal results", {
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
    set.seed(42)
    settings_matrix <- generate_settings_matrix(
        data_list,
        nrow = 3,
        max_k = 40
    )
    # This matrix has clustering solutions for each of the 5 SNF runs!
    solutions_matrix <- batch_snf(data_list, settings_matrix)
    solutions_matrix_parallel <- batch_snf(
        data_list,
        settings_matrix,
        processes = 2,
    )
    expect_equal(solutions_matrix, solutions_matrix_parallel)
})
