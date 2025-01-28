# 1. subsample_dl: Return a list of subsampled (without replacement) data lists
# 2. batch_snf_subsamples: Convert a list of subsampled data lists into a list of cluster solutions
# 3.
devtools::load_all()
library(sloop)
library(testthat)

#' #dl <- data_list(
#' #    list(income, "household_income", "demographics", "ordinal"),
#' #    list(pubertal, "pubertal_status", "demographics", "continuous"),
#' #    list(fav_colour, "favourite_colour", "demographics", "categorical"),
#' #    list(anxiety, "anxiety", "behaviour", "ordinal"),
#' #    list(depress, "depressed", "behaviour", "ordinal"),
#' #    uid = "unique_id"
#' #)
#' #
#' #sc <- snf_config(
#' #    dl,
#' #    n_solutions = 4,
#' #    dropout_dist = "uniform",
#' #    max_k = 50
#' #)
#' #
#' #sol_df <- batch_snf(dl, sc)
#' #
#' #ext_sol_df <- extend_solutions(sol_df, dl)
#' #
#' #pval_heatmap(ext_sol_df)


