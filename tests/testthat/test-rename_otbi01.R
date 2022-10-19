test_that("rename_otbi01() does not leave behind ambiguous colnames", {
    # Mock abcd_otbi01.txt
    abcd_otbi01 <- data.frame(matrix(NA, nrow = 2, ncol = 38))
    original_colnames <- c(
        "tbi_1", "tbi_1b", "tbi_1c", "tbi_1d", "tbi_2", "tbi_2b", "tbi_2c",
        "tbi_2d", "tbi_3", "tbi_3b", "tbi_3c", "tbi_3d", "tbi_4", "tbi_4b",
        "tbi_4c", "tbi_4d", "tbi_5", "tbi_5b", "tbi_5c", "tbi_5d", "tbi_6o",
        "tbi_6p", "tbi_6q", "tbi_6r", "tbi_6s", "tbi_7a", "tbi_7c1",
        "tbl_7c2", "tbi_7e", "tbi_7f", "tbi_7g", "tbi_7i", "tbi_7k",
        "tbi_7l", "tbi_8g", "tbi_8i", "tbi_8k", "tbi_8l")
    colnames(abcd_otbi01) <- original_colnames
    otbi01_renamed <- rename_otbi01(abcd_otbi01)
    num_unnamed_cols <- sum(original_colnames %in% colnames(otbi01_renamed))
    expect_equal(num_unnamed_cols, 0)
})

test_that("rename_otbi01() warns if nothing was changed", {
    # Mock abcd_otbi01.txt
    abcd_otbi01 <- data.frame(matrix(NA, nrow = 2, ncol = 38))
    original_colnames <- c(
        "tbi_1", "tbi_1b", "tbi_1c", "tbi_1d", "tbi_2", "tbi_2b", "tbi_2c",
        "tbi_2d", "tbi_3", "tbi_3b", "tbi_3c", "tbi_3d", "tbi_4", "tbi_4b",
        "tbi_4c", "tbi_4d", "tbi_5", "tbi_5b", "tbi_5c", "tbi_5d", "tbi_6o",
        "tbi_6p", "tbi_6q", "tbi_6r", "tbi_6s", "tbi_7a", "tbi_7c1",
        "tbl_7c2", "tbi_7e", "tbi_7f", "tbi_7g", "tbi_7i", "tbi_7k",
        "tbi_7l", "tbi_8g", "tbi_8i", "tbi_8k", "tbi_8l")
    colnames(abcd_otbi01) <- original_colnames
    otbi01_renamed <- rename_otbi01(abcd_otbi01)
    expect_warning(rename_otbi01(otbi01_renamed), class = "no_effect")
})

test_that("rename_otbi01() errors if argument is not a data.frame", {
    expect_error(rename_otbi01(NA), class = "non_df")
})

test_that("original_otbi_names() prints expected table of old otbi01 colnames", {
    expect_snapshot(original_otbi_names())
})
