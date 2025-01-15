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

sol_aris <- calc_aris(sol_df)

mc_order <- get_matrix_order(sol_aris)

split_vec <- c(9, 11, 13, 15, 18)
ari_hm <- adjusted_rand_index_heatmap(
    sol_aris,
    order = mc_order,
    split_vector = split_vec
)
ari_hm

z <- label_meta_clusters(
    sol_df,
    split_vec,
    mc_order
)

z

ari_hm <- adjusted_rand_index_heatmap(
    sol_aris[mc_order, mc_order],
    split_vector = split_vec,
    show_row_names = TRUE
)
ari_hm

label_meta_clusters <- function(sol_df, split_vector, order = NULL) {
    n_solutions <- nrow(sol_df)
    labels <- rep("A", n_solutions)
    if (split_vector[length(split_vector)] != n_solutions) {
        split_vector <- c(split_vector, n_solutions)
    }
    for (i in 1:(length(split_vector) - 1)) {
        start <- split_vector[i]
        end <- split_vector[i + 1]
        labels[start:end] <- LETTERS[i + 1]
    }
    if (is.null(order)) {
        order <- seq_len(nrow(sol_df))
    }
    sol_df_ordered <- sol_df[order, ]
    corresponding_solutions <- sol_df_ordered$"solution"
    sol_df_ordered$"mc" <- labels
    sol_df <- dplyr::select(sol_df_ordered, solution, nclust, mc, dplyr::everything())
    sol_df <- sol_df[sort(sol_df$"solution", index.return = TRUE)$ix, ]
    return(sol_df)
}

label_meta_clusters(sol_df, split_vec, mc_order)
