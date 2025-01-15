devtools::load_all()
library(sloop)
library(testthat)

set.seed(42)
dl <- data_list(
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)

sc <- snf_config(input_dl, n_solutions = 5)
sol_df <- batch_snf(input_dl, sc, return_sim_mats = TRUE)
aris <- calc_aris(sol_df)

split <- c(1, 3)

get_representative_solutions(
    aris = aris,
    split_vector = split,
    sol_df = sol_df
)

sim_mat <- sim_mats_list(sol_df)[[1]]
estimate_nclust_given_graph(sim_mat)
