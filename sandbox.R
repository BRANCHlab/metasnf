#library(sloop)
#library(testthat)
#library(metasnf)
# 1. subsample_dl: Return a list of subsampled (without replacement) data lists
# 2. batch_snf_subsamples: Convert a list of subsampled data lists into a list of cluster solutions
# 3.

devtools::load_all()

dl <- data_list(
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    list(depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)

sc <- snf_config(
    dl = dl,
    n_solutions = 20,
    min_k = 20,
    max_k = 50
)

sol_df <- batch_snf(dl, sc)

ext_sol_df <- extend_solutions(
    sol_df,
    dl = dl,
    min_pval = 1e-10 # p-values below 1e-10 will be thresholded to 1e-10
)

# Calculate pairwise similarities between cluster solutions
sol_aris <- calc_aris(sol_df)

# Extract hierarchical clustering order of the cluster solutions
meta_cluster_order <- get_matrix_order(sol_aris)

# Identify meta cluster boundaries with shiny app or trial and error
# ari_hm <- meta_cluster_heatmap(sol_aris, order = meta_cluster_order)
# shiny_annotator(ari_hm)

# Result of meta cluster examination
split_vec <- c(2, 5, 12, 17)

ext_sol_df <- label_meta_clusters(ext_sol_df, split_vec, meta_cluster_order)

# Extracting representative solutions from each defined meta cluster
rep_solutions <- get_representative_solutions(sol_aris, ext_sol_df)

mc_manhattan <- mc_manhattan_plot(
    rep_solutions,
    dl = dl,
    point_size = 3,
    text_size = 12,
    plot_title = "Feature-Meta Cluster Associations",
    threshold = 0.05,
    neg_log_pval_thresh = 5
)

mc_manhattan
