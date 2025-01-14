devtools::load_all()
set.seed(44)
library(sloop)
library(testthat)

z <- get_complete_uids(
    list(
        subc_v,
        income,
        anxiety,
        depress
    ),
    uid = "unique_id"
)

input_dl <- data_list(
    list(dplyr::filter(subc_v, unique_id %in% z), "scv", "neuroimaging", "continuous"),
    list(dplyr::filter(income, unique_id %in% z), "income", "neuroimaging", "continuous"),
    uid = "unique_id"
)

target_dl <- data_list(
    list(dplyr::filter(anxiety, unique_id %in% z), "anxiety", "behaviour", "ordinal"),
    list(dplyr::filter(depress, unique_id %in% z), "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)

config <- snf_config(
    input_dl,
    n_solutions = 5
)

sol_df <- batch_snf(input_dl, config)

ext_sol_df <- extend_solutions(
    sol_df,
    target_dl = target_dl
)
