devtools::load_all()

input_dl <- data_list(
    list(gender_df, "gender", "demographics", "categorical"),
    list(diagnosis_df, "diagnosis", "clinical", "categorical"),
    uid = "patient_id"
)

sc <- snf_config(input_dl, n_solutions = 2)

sol_df <- batch_snf(input_dl, sc)

ext_sol_df <- extend_solutions(sol_df, input_dl)


attributes(ext_sol_df) |> names()

attributes(ext_sol_df[1, ]) |> names()
