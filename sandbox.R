devtools::load_all()
library(sloop)
library(testthat)

set.seed(42)
input_dl <- data_list(
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)

sc <- snf_config(input_dl, n_solutions = 4)

sol_df <- batch_snf(input_dl, sc)

sol_df


calc_aris(sol_df)

ext_sol_df <- extend_solutions(sol_df, dl = input_dl)

ext_sol_df2 <- extend_solutions(sol_df, target_dl = input_dl)
