# merge_df_list ################################################################
test_that(
    "ensure that two dataframes can be inner joined properly",
    {
        df1 <- data.frame(
            uid = c("a", "b", "c"),
            var1 = c(1, 2, 3)
        )
        df2 <- data.frame(
            uid = c("a", "b"),
            var2 = c(4, 5)
        )
        df3 <- data.frame(
            uid = c("a", "b"),
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
            uid = c("a", "b", "c"),
            var1 = c(1, 2, 3)
        )
        df2 <- data.frame(
            uid = c("a", "b"),
            var2 = c(4, 5)
        )
        df3 <- data.frame(
            uid = c("a", "b", "c"),
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

# add_columns
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
