#library(metasnf)
#library(testthat)

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
            add_columns(df, newcols, fill),
            regexp = "`fill` parameter"
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
            add_columns(df, newcols, fill),
            regexp = "`newcols` parameter"
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
            A = c(1, 2, "apple"),
            B = c(1, 2, 3)
        )
        expect_identical(
            numcol_to_numeric(df),
            df
        )
    }
)
################################################################################

# char_to_fac ##################################################################
test_that(
    paste0(
        "ensure that the character columns of a dataframe can be converted to",
        " factor columns"
    ),
    {
        df1 <- data.frame(
            A = c("dog", "cheese", "bird"),
            B = c(1, 2, 3)
        )
        df2 <- data.frame(
            A = factor(c("dog", "cheese", "bird")),
            B = c(1, 2, 3)
        )
        expect_identical(
            char_to_fac(df1),
            df2
        )
    }
)
################################################################################

# no_subs ######################################################################
test_that(
    "ensure that columns starting with subject_ are removed from a dataframe",
    {
        df1 <- data.frame(
            row_id = c(1, 2, 3),
            A = c(1, 2, 3),
            B = c(1, 2, 3),
            subject_1 = c(1, 2, 3),
            subject_2 = c(1, 2, 3)
        )
        df2 <- data.frame(
            row_id = c(1, 2, 3),
            A = c(1, 2, 3),
            B = c(1, 2, 3)
        )
        expect_equal(
            no_subs(df1),
            df2
        )
    }
)

test_that(
    "ensure that dataframes without any 'subject_' columns raise a warning",
    {
        df <- data.frame(
            row_id = c(1, 2, 3),
            A = c(1, 2, 3),
            B = c(1, 2, 3)
        )
        expect_warning(
            no_subs(df),
            regexp = "no 'subject_'"
        )
    }
)
################################################################################

# subs ######################################################################
test_that(
    "ensure columns not starting with subject_ are removed from a dataframe",
    {
        df1 <- data.frame(
            row_id = c(1, 2, 3),
            A = c(1, 2, 3),
            B = c(1, 2, 3),
            subject_1 = c(1, 2, 3),
            subject_2 = c(1, 2, 3)
        )
        df2 <- data.frame(
            row_id = c(1, 2, 3),
            subject_1 = c(1, 2, 3),
            subject_2 = c(1, 2, 3)
        )
        expect_equal(
            subs(df1),
            df2
        )
    }
)

test_that(
    "ensure dataframes without 'row_id' column raises error",
    {
        df <- data.frame(
            A = c(1, 2, 3),
            B = c(1, 2, 3),
            subject_1 = c(1, 2, 3),
            subject_2 = c(1, 2, 3)
        )
        expect_error(
            subs(df),
            regexp = "row_id"
        )
    }
)

test_that(
    "ensure that dataframes with only 'subject_' columns raise a warning",
    {
        df <- data.frame(
            row_id = c(1, 2, 3),
            subject_A = c(1, 2, 3),
            subject_B = c(1, 2, 3)
        )
        expect_warning(
            subs(df),
            regexp = "no non-'subject_'"
        )
    }
)
################################################################################

# merge_df_list ################################################################
test_that(
    "ensure that two dataframes can be inner joined properly",
    {
        df1 <- data.frame(
            subjectkey = c("a", "b", "c"),
            var1 = c(1, 2, 3)
        )
        df2 <- data.frame(
            subjectkey = c("a", "b"),
            var2 = c(4, 5)
        )
        df3 <- data.frame(
            subjectkey = c("a", "b"),
            var1 = c(1, 2),
            var2 = c(4, 5)
        )
        expect_equal(
            merge_df_list(list(df1, df2), join = "inner"),
            df3
        )
    }
)

test_that(
    "ensure that two dataframes can be full joined properly",
    {
        df1 <- data.frame(
            subjectkey = c("a", "b", "c"),
            var1 = c(1, 2, 3)
        )
        df2 <- data.frame(
            subjectkey = c("a", "b"),
            var2 = c(4, 5)
        )
        df3 <- data.frame(
            subjectkey = c("a", "b", "c"),
            var1 = c(1, 2, 3),
            var2 = c(4, 5, NA)
        )
        expect_equal(
            merge_df_list(list(df1, df2), join = "full"),
            df3
        )
    }
)
################################################################################

# train_test_assign ############################################################
test_that(
    "ensure constant output from train_test_assign",
    {
        split_results <- train_test_assign(
            0.5,
            c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
        )
        train_subs <- split_results$train
        test_subs <- split_results$test
        correct_train <- identical(
            train_subs,
            c("b", "c", "d", "e", "h", "i")
        )
        correct_test <- identical(
            test_subs,
            c("a", "f", "g", "j")
        )
        both_correct <- correct_train && correct_test
        expect_true(both_correct)
    }
)

test_that(
    "ensure constant output from train_test_assign",
    {
        expect_warning(
            train_test_assign(
                0.9,
                c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
            ),
            regexp = "Empty train or test set"
        )
    }
)
################################################################################

# keep_split ###################################################################
# No test needed - this is a very straight forward utility function
################################################################################
