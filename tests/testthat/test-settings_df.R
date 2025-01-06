#devtools::load_all()
#library(testthat)

###############################################################################
# settings_df()
###############################################################################
test_that("return a correctly formatted settings_df", {
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
    data_list <- data_list(
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
    empty_settings_df <- settings_df(data_list)
    empty_has_no_rows <- nrow(empty_settings_df) == 0
    proper_colnames <- (colnames(empty_settings_df) == c(
        "solution",
        "alpha",
        "k",
        "t",
        "snf_scheme",
        "clust_alg",
        "cnt_dist",
        "dsc_dist",
        "ord_dist",
        "cat_dist",
        "mix_dist",
        "inc_heart_rate",
        "inc_personality_test"
    )) |> all()
    good_format <- empty_has_no_rows & proper_colnames
    expect_equal(TRUE, good_format)
})

