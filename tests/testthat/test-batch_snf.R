#devtools::load_all()
#library(testthat)

###############################################################################
# batch_snf()
###############################################################################
test_that("function and parallel equivalent run and give equal results", {
    # Load the package
    library(metasnf)
    # Setting up the data
    dl <- data_list(
        list(abcd_income, "household_income", "demographics", "continuous"),
        list(abcd_pubertal, "pubertal_status", "demographics", "continuous"),
        uid = "patient"
    )
    # Specifying 5 different sets of settings for SNF
    set.seed(42)
    config <- snf_config(
        dl,
        n_solutions = 2,
        max_k = 40
    )
    # This matrix has clustering solutions for each of the 5 SNF runs!
    solutions_df <- batch_snf(dl, config)
    solutions_df_parallel <- batch_snf(dl, config, processes = 2)
    expect_equal(solutions_df, solutions_df_parallel)
})
