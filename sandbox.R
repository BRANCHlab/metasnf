devtools::load_all()
set.seed(44)
library(sloop)
library(testthat)

dl <- data_list(
    list(subc_v, "scv", "neuroimaging", "continuous"),
    list(cort_t, "cort_t", "neuroimaging", "continuous"),
    list(cort_sa, "cort_sa", "neuroimaging", "continuous"),
    list(income, "income", "neuroimaging", "continuous"),
    uid = "unique_id"
)

config <- snf_config(
    dl,
    n_solutions = 40,
    cnt_dist_fns = list(
        "siw_euclidean" = siw_euclidean_distance,
        "sew_euclidean" = sew_euclidean_distance
    ),
    use_default_dist_fns = TRUE
)

config







start <- Sys.time()
with_progress(
    sol_df <- batch_snf2(dl, config, return_sim_mats = FALSE, processes = 1)
)
print(Sys.time() - start)

data.frame(sol_df)

print(sol_df, t = TRUE)

class(sol_df)

attributes(sol_df)

tibble::tibble(as.data.frame(sol_df))

class(sr)

tibble::tibble(income)

tibble::tibble(sol_df)

sr |> attributes()

attributes(sr)$"sim_mats_list"

class(sr$"solutions_df")

#start <- Sys.time()
#sol_df <- batch_snf(dl, config$"settings_df", return_similarity_matrices = TRUE)
#print(Sys.time() - start)

# Printing out snf results / solutions df!

