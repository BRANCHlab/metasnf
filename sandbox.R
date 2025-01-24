# 1. subsample_dl: Return a list of subsampled (without replacement) data lists
# 2. batch_snf_subsamples: Convert a list of subsampled data lists into a list of cluster solutions
# 3.
devtools::load_all()
library(sloop)
library(testthat)

my_dl <- data_list(
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)

sc <- snf_config(
    my_dl,
    n_solutions = 10,
    max_k = 40,
    weights_fill = "uniform"
)
sc

config_heatmap(sc = sc)


# To do regarding config heatmap
# - option to drop static columns and annotations
# - automatic split markers at appropriate points
# - option to turn ON/OFF for settings_df vs. weights matrix

sc$"settings_df" |> colnames()


library(RColorBrewer)
display.brewer.all(type = "qual")






sol_df <- batch_snf(my_dl, sc)

sol_aris <- calc_aris(sol_df)

ext_sol_df <- extend_solutions(sol_df, dl = my_dl)

attributes(ext_sol_df)

sol_df |> class()


attributes(ext_sol_df)$"snf_config" <- attributes(sol_df)$"snf_config"

attributes(dplyr::select(sol_df, dplyr::everything())) |> names()

dplyr::select(sol_df, dplyr::everything())

dplyr::select(ext_sol_df, dplyr::everything())

attributes(ext_sol_df[, ]) |> names()

class(sol_df) <- c("ext_solutions_df", "data.frame")

attributes(sol_df) |> names()


undebug(extend_solutions)

class(sol_df)


critical_cols <- c("solution", "nclust", "mc")

all(critical_cols %in% colnames(sol_df))

