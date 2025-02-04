#------------------------------------------------------------------------------
# data_list()
#------------------------------------------------------------------------------
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

#------------------------------------------------------------------------------
# convert_uids()
#------------------------------------------------------------------------------
test_that(
    "Properly converts UIDs of a data list in preparation to 'uid'.",
    {
        # Create a data list-like list
        dll <- data_list(
            list(abcd_income, "name", "neuroimaging", "continuous"),
            uid = "patient"
        ) |>
            lapply(
                function(x) {
                    x
                }
            )
        # Create a data list-like list with wrong UID
        wrong_uid_dll <- lapply(
            dll,
            function(x) {
                x$"data" <- dplyr::rename(
                    x$"data",
                    "patient" = "uid"
                )
                return(x)
            }
        ) 
        # Check if convert_uids makes the two equal
        expect_equal(
            dll,
            metasnf:::convert_uids(wrong_uid_dll, "patient")
        )
    }
)

test_that(
    "Errors when UID column is not unique.",
    {
        no_uid_df <- abcd_cort_t
        no_uid_df[1:2, "patient"] <- "x"
        expect_error(
            dl <- data_list(
                list(no_uid_df, "name", "domain", "continuous"),
                uid = "patient"
            ),
            "does not uniquely ID all observations"
        )
    }
)
