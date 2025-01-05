devtools::load_all()
set.seed(44)
library(sloop)
library(testthat)

input_dl <- data_list(
    list(subc_v, "scv", "neuroimaging", "continuous"),
    list(income, "income", "neuroimaging", "continuous"),
    uid = "unique_id"
)

target_dl <- data_list(
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    list(depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)

config <- snf_config(
    input_dl,
    n_solutions = 5
)

sol_df <- batch_snf(input_dl, config)

debug(extend_solutions)

ext_sol_df <- extend_solutions(
    sol_df,
    target_dl = target_dl
)
