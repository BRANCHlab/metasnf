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
sc <- snf_config(my_dl, n_solutions = 3, max_k = 40)
sol_df <- batch_snf(my_dl, sc)
#
my_dl_subsamples <- subsample_dl(
    my_dl,
    n_subsamples = 10,
    subsample_fraction = 0.85
)
#
subsampled_solutions <- batch_snf_subsamples(
    my_dl_subsamples,
    sc,
    verbose = TRUE
)
#subsampled_solutions
#pairwise_aris <- subsample_pairwise_aris(
#    subsampled_solutions,
#    return_raw_aris = TRUE,
#    verbose = TRUE
#)
#pairwise_aris$"ari_summary"
#pairwise_aris$"raw_aris"$"s1"

start <- Sys.time()
coclustering_results <- calculate_coclustering(
    subsampled_solutions,
    sol_df,
    verbose = TRUE
)
print(Sys.time() - start)


lapply(
    sol_df,
)

lapply(
    seq_len(nrow(sol_df)),
    function(i, cluster_solutions, base_cocluster_df) {
        current_solution <- cluster_solutions[, c(1, i + 1)] 
        colnames(current_solution) <- c("uid", "cluster")
        cocluster_df <- base_cocluster_df
        colnames()
        head(cocluster_df)
    },
    cluster_solutions = t(sol_df),
    base_cocluster_df = data.frame(t(utils::combn(t(sol_df)$"uid", 2)))
)

z$"cocluster_df"[[1]] |> head()

start <- Sys.time()
z2 <- calculate_coclustering2(
    subsampled_solutions,
    sol_df,
    verbose = TRUE
)
print(Sys.time() - start)

start <- Sys.time()
z <- calculate_coclustering(
    subsampled_solutions,
    sol_df,
    verbose = TRUE
)
print(Sys.time() - start)






# 
# 21.71

all.equal(z, coclustering_results)


lapply(subsampled_solutions, t)


coclustering_results

coclustering_results$"cocluster_summary"

cocluster_dfs <- coclustering_results$"cocluster_dfs"

cocluster_density(cocluster_dfs[[1]])

cocluster_heatmap(
    cocluster_dfs[[1]],
    dl = my_dl,
    top_hm = list(
        "Income" = "household_income",
        "Pubertal Status" = "pubertal_status"
    ),
    annotation_colours = list(
        "Pubertal Status" = colour_scale(
            c(1, 4),
            min_colour = "black",
            max_colour = "purple"
        ),
        "Income" = colour_scale(
            c(0, 4),
            min_colour = "black",
            max_colour = "red"
        )
    )
)
