# 1. subsample_dl: Return a list of subsampled (without replacement) data lists
# 2. batch_snf_subsamples: Convert a list of subsampled data lists into a list of cluster solutions
# 3.
devtools::load_all()
library(sloop)
library(testthat)

set.seed(43)
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
