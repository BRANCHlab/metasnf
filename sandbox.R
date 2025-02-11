devtools::load_all()

input_dl <- data_list(
    list(gender_df, "gender", "demographics", "categorical"),
    list(diagnosis_df, "diagnosis", "clinical", "categorical"),
    uid = "patient_id"
)

sc <- snf_config(input_dl, n_solutions = 10)

sol_df <- batch_snf(input_dl, sc, return_sim_mats = TRUE)

sol_df2 <- batch_snf(input_dl, sc, return_sim_mats = TRUE)
