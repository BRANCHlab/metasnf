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

sc <- snf_config(input_dl, n_solutions = 20)

sol_df <- batch_snf(input_dl, sc, return_sim_mats = TRUE)

ext_sol_df <- extend_solutions(sol_df, dl = input_dl)


    common_cols <- c("solution", "nclust", "mc")
    summary_cols_1 <- c("min_pval", "mean_pval", "max_pval")
    summary_cols_2 <- NULL

ext_sol_df |>
    dplyr::select(dplyr::all_of(common_cols), dplyr::all_of(summary_cols_2)) |>
    tibble::tibble()

ext_sol_df$"min_pval"

ext_sol_df

class(ext_sol_df)

uids(sol_df)

mc_order <- get_matrix_order(sol_aris)

split_vec <- c(9, 11, 13, 15, 18)
ari_hm <- adjusted_rand_index_heatmap(
    sol_aris,
    order = mc_order,
    split_vector = split_vec
)
ari_hm

zz <- label_meta_clusters(
    sol_df,
    split_vec,
    mc_order
)

ari_hm <- adjusted_rand_index_heatmap(
    sol_aris[mc_order, mc_order],
    split_vector = split_vec,
    show_row_names = TRUE
)
ari_hm
