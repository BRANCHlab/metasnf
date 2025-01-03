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
    n_solutions = 5,
    cnt_dist_fns = list(
        "siw_euclidean" = siw_euclidean_distance,
        "sew_euclidean" = sew_euclidean_distance
    ),
    use_default_dist_fns = TRUE
)

with_progress(
    sol_df <- batch_snf(dl, config, return_sim_mats = FALSE, processes = 1)
)

devtools::load_all()
z <- batch_nmi(dl, sol_df)

feature_dl

paste0("inc_", feature_dl[[1]]$"name")


class(config) <- "snf_config"

nrow(config)

getS3method("nrow", class())

exists("nrow.snf_config")

is.function(nrow)

methods("nrow")

sloop::s3_dispatch(nrow(config))

nrow.snf_config(config)

class(config) <- ""

z |>
    dlapply(
        function(x) {
        }
    )
