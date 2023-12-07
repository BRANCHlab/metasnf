###############################################################################
# generate_data_list()
###############################################################################
test_that("return a correctly formatted data_list", {
    heart_rate_df <- data.frame(
        patient_id = c("1", "2", "3"),
        var1 = c(0.04, 0.1, 0.3),
        var2 = c(30, 2, 0.3)
    )
    personality_test_df <- data.frame(
        patient_id = c("1", "2", "3"),
        var3 = c(900, 1990, 373),
        var4 = c(509, 2209, 83)
    )
    data_list <- generate_data_list(
        list(
            data = heart_rate_df,
            name = "heart_rate",
            domain = "clinical",
            type = "continuous"
        ),
        list(
            data = personality_test_df,
            name = "personality_test",
            domain = "surveys",
            type = "continuous"
        ),
        uid = "patient_id"
    )
    expect_equal(
        TRUE,
        all(
            c(
                sorted_names,
                data_is_df,
                correct_component_names
            )
        )
    )
})

