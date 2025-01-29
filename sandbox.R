#library(sloop)
#library(testthat)
#library(metasnf)
# 1. subsample_dl: Return a list of subsampled (without replacement) data lists
# 2. batch_snf_subsamples: Convert a list of subsampled data lists into a list of cluster solutions
# 3.
devtools::load_all()
dl <- data_list(
    list(cort_sa, "cortical_surface_area", "neuroimaging", "continuous"),
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)
set.seed(42)
my_sc <- snf_config(
    dl = dl,
    n_solutions = 4,
    min_k = 20,
    max_k = 50
)
sol_df <- batch_snf(dl, my_sc, return_sim_mats = TRUE)
ext_sol_df <- extend_solutions(sol_df, dl)

ext_sol_df2 <- extend_solutions2(sol_df, dl)

identical(
    sim_mats_list(ext_sol_df[c(1, 3), ])[[2]],
    sim_mats_list(ext_sol_df)[[3]]
)

unique(c("unique_id", "smri_vol_scs_aar", colnames(subc_v)))

ext_sol_df[, !(colnames(ext_sol_df) %in% "solution")]



