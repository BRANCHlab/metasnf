devtools::load_all()

input_dl <- data_list(
    list(gender_df, "gender", "demographics", "categorical"),
    list(diagnosis_df, "diagnosis", "clinical", "categorical"),
    uid = "patient_id"
)

sc <- snf_config(input_dl, n_solutions = 10)

sol_df <- batch_snf(input_dl, sc, return_sim_mats = TRUE)

ext_sol_df <- extend_solutions(sol_df, input_dl)

# In anticipation of the upcoming cluster swaps
cluster_label_swap <- function(solutions_matrix, c_a, c_b) {
    swapped_sm <- dplyr::mutate(
        solutions_matrix,
        across(
            dplyr::starts_with("subject"),
            ~ ifelse(
                . == c_a,
                c_b,
                ifelse(
                    . == c_b,
                    c_a,
                    .
                )
            )
        )
    )
    return(swapped_sm)
}

attributes(ext_sol_df) |> names()

z1 <- attributes(cluster_label_swap(ext_sol_df, 2, 3))
z2 <- attributes(ext_sol_df)



