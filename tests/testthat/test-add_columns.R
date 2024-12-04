test_that(
    "extend a dataframe with new columns",
    {
        df <- data.frame(A = c(1, 2, 3))
        newcols <- c("B", "C", "D")
        fill <- "apple"
        expect_equal(
            add_columns(df, newcols, fill),
            data.frame(
                A = c(1, 2, 3),
                B = c("apple", "apple", "apple"),
                C = c("apple", "apple", "apple"),
                D = c("apple", "apple", "apple")
            )
        )
    }
)
