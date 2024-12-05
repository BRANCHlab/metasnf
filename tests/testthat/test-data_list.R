###############################################################################
# data_list()
###############################################################################
test_that("Return a correctly formatted data_list.", {
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
    correct_component_names <- data_list |> lapply(
        function(x) {
            all(names(x) == c("data", "name", "domain", "type"))
        }
    ) |>
        unlist() |>
        all()
    data_is_df <- data_list |> lapply(
        function(x) {
            all(inherits(x$"data", "data.frame"))
        }
    ) |>
        unlist() |>
        all()
    expect_equal(
        TRUE,
        all(
            c(
                data_is_df,
                correct_component_names
            )
        )
    )
})

test_that(
    "Errors on partial specification of component names.",
    {
        uid <- LETTERS
        df <- data.frame(uid, a = seq_along(LETTERS))
        expect_error(
            data_list <- data_list(
                list(df, "name", "domain", type = "discrete")
            ),
            "for all of the elements or for none of them"
        )
    }
)

test_that(
    "Errors on partial specification of component names.",
    {
        uid <- LETTERS
        df <- data.frame(uid, a = seq_along(LETTERS))
        expect_error(
            data_list <- data_list(
                list(df, "name", "domain", type = "discrete")
            ),
            "for all of the elements or for none of them"
        )
    }
)

test_that(
    "Errors when UID column is not unique.",
    {
        abcd_cort_t[1:2, "patient"] <- "x"
        expect_error(
            dl <- data_list(
                list(abcd_cort_t, "name", "domain", "continuous"),
                uid = "patient"
            ),
            "does not uniquely ID all observations"
        )
    }
)

###############################################################################
# convert_uids()
###############################################################################
#test_that(
#    "Properly converts UIDs of a data list in preparation to 'uid'.",
#    {
#        dl <- data_list(
#            list(abcd_income, "name", "neuroimaging", "continuous"),
#            uid = "patient"
#        )
#        wrong_uid_dl <- lapply(
#            dl,
#            function(x) {
#                x$"data" <- dplyr::rename(
#                    x$"data",
#                    "patient" = "uid"
#                )
#                return(x)
#            }
#        ) 
#        expect_equal(
#            dl, metasnf:::convert_uids(wrong_uid_dl, "patient")
#        )
#    }
#)


###############################################################################
# new_data_list()
###############################################################################
test_that(
    "Developer constructor for a data list.",
    {
        expect_true(
            inherits(metasnf:::new_data_list(list(list())), "data_list")
        )
    }
)
