# library(metasnf)
# library(testthat)

# add_columns ##################################################################
test_that(
    "extend a dataframe with columns in `newcols` containing the `fill` value",
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

test_that(
    "error if the fill parameter is NULL",
    {
        df <- data.frame(A = c(1, 2, 3))
        newcols <- c("B", "C", "D")
        fill <- NULL
        expect_error(
            add_columns(df, newcols, fill)
        )
    }
)

test_that(
    "warn if the newcols are not provided as a character vector",
    {
        df <- data.frame(A = c(1, 2, 3))
        newcols <- c(1, 2, 3)
        fill <- "apple"
        expect_warning(
            add_columns(df, newcols, fill)
        )
    }
)
################################################################################

# numcol_to_numeric ############################################################
test_that(
    "ensure that a non-numeric column of numbers can be converted to numeric",
    {
        df1 <- data.frame(
            A = c("1", "2", "3"),
            B = c(1, 2, 3)
        )
        df2 <- data.frame(
            A = c(1, 2, 3),
            B = c(1, 2, 3)
        )
        expect_equal(
            numcol_to_numeric(df1),
            df2
        )
    }
)

test_that(
    "ensure dataframe is left unchanged if no columns can be converted",
    {
        df <- data.frame(
            A = c(1, 2, "cat"),
            B = c(1, 2, 3)
        )
        expect_identical(
            numcol_to_numeric(df),
            df
        )
    }
)
