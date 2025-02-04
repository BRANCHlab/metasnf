library(metasnf)


input_dl <- data_list(
    list(subc_v, "subcortical_volume", "behaviour", "ordinal"),
    list(income, "income", "neuroimaging", "continuous"),
    uid = "unique_id"
)

sc <- snf_config(input_dl, n_solutions = 1)

sol_df <- batch_snf(input_dl, sc)

ext_sol_df <- extend_solutions(
    sol_df,
    target_dl = target_dl
)
