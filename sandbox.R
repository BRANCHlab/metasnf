library(metasnf)
library(ggplot2)

data_list <- generate_data_list(
    list(cort_t, "cortical_thickness", "neuroimaging", "continuous"),
    list(cort_sa, "cortical_area", "neuroimaging", "continuous"),
    list(subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(income, "household_income", "demographics", "continuous"),
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "unique_id"
)

settings_matrix <- generate_settings_matrix(
    data_list,
    nrow = 4,
    max_k = 40,
    seed = 42
)

solutions_matrix <- batch_snf(data_list, settings_matrix)

set.seed(42)
data_list_subsamples <- subsample_data_list(
    data_list,
    n_subsamples = 100,
    subsample_fraction = 0.85
)

batch_subsample_results <- batch_snf_subsamples(
    data_list_subsamples,
    settings_matrix
)

names(batch_subsample_results)

subsample_cluster_solutions <- batch_subsample_results[["cluster_solutions"]]
pairwise_aris <- subsample_pairwise_aris(
    subsample_cluster_solutions,
    return_raw_aris = TRUE
)

names(pairwise_aris)

ari_summary <- pairwise_aris$"ari_summary"
print(ari_summary)

raw_aris <- pairwise_aris$"raw_aris"

length(raw_aris)

raw_aris[[1]][1:5, 1:5]

ss_ari_hm <- ComplexHeatmap::Heatmap(
    raw_aris[[1]],
    heatmap_legend_param = list(
        color_bar = "continuous",
        title = "Inter-Subsample\nARI",
        at = c(0, 0.5, 1)
    ),
    show_column_names = FALSE,
    show_row_names = FALSE
)

save_heatmap(
    heatmap = ss_ari_hm,
    path = "figures/ss_ari_hm.png",
    width = 600,
    height = 500,
    res = 100
)


# scales linearly with the number of subsamples and quadratically with the number of
# observations.

coclustering_results <- calculate_coclustering(
    subsample_cluster_solutions,
    solutions_matrix
)

names(coclustering_results)

cocluster_dfs <- coclustering_results[["cocluster_dfs"]]
head(cocluster_dfs[[1]])

cocluster_summary <- coclustering_results[["cocluster_summary"]]
print(cocluster_summary)

cocluster_dfs[[1]] |> head()

density_plot <- cocluster_density(cocluster_dfs[[1]])

density_plot

ggsave(
    "figures/cocluster_density.png",
    density_plot,
    width = 6,
    height = 5
)

cc_hm <- cocluster_heatmap(
    cocluster_dfs[[1]],
    data_list = data_list,
    top_hm = list(
        "Income" = "household_income",
        "Pubertal Status" = "pubertal_status"
    )
)

save_heatmap(
    heatmap = cc_hm,
    path = "figures/cc_hm.png",
    width = 750,
    height = 500,
    res = 100
)

```
