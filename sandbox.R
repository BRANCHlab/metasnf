# Note: when testing optional packages, see https://r-pkgs.org/dependencies-in-practice.html#sec-dependencies-in-suggests-in-tests

library(metasnf)

data_list <- generate_data_list(
    list(abcd_cort_t, "cortical_thickness", "neuroimaging", "numeric"),
    list(abcd_cort_sa, "cortical_surface_area", "neuroimaging", "numeric"),
    list(abcd_subc_v, "subcortical_volume", "neuroimaging", "numeric"),
    list(abcd_income, "household_income", "demographics", "numeric"),
    list(abcd_pubertal, "pubertal_status", "demographics", "numeric"),
    old_uid = "patient"
)

settings_matrix <- generate_settings_matrix(
    data_list,
    nrow = 5,
    max_k = 40,
    seed = 42
)

batch_snf_results <- batch_snf(
    data_list,
    settings_matrix,
    return_affinity_matrices = TRUE
)

solutions_matrix <- batch_snf_results$"solutions_matrix"
affinity_matrices <- batch_snf_results$"affinity_matrices"

data_list_subsamples <- subsample_data_list(
    data_list,
    n_subsamples = 3,
    subsample_fraction = 0.8
)


subsample_solutions <- subsample_pairwise_aris(data_list_subsamples, settings_matrix)


subsample_solutions[[1]]

# Dataframe containing the cluster solutions from the full data_list
full_cluster_solutions <- get_cluster_solutions(solutions_matrix)

full_cluster_solutions

# The solution we are interested in
i <- 1

current_solution_df <- full_cluster_solutions[, c(1, i + 1)]
colnames(current_solution_df) <- c("subjectkey", "cluster")

current_solution <- current_solution_df[, 2]

unique_clusters <- current_solution |>
    unique() |>
    sort()

subs_grouped_by_cluster <- unique_clusters |>
    lapply(
        function(cluster) {
            current_solution_df[current_solution_df$"cluster" %in% cluster, ]
        }
    )

clustered_pairs_df_list <- subs_grouped_by_cluster |>
    lapply(
        function(cluster_group) {
            clustered_pairs <- cluster_group$"subjectkey" |>
                combn(2) |>
                t() |>
                data.frame()
            colnames(clustered_pairs) <- c("subject_1", "subject_2")
            rownames(clustered_pairs) <- NULL
            return(clustered_pairs)
        }
    )

clustered_pairs_df <- do.call("rbind", clustered_pairs_df_list)

clustered_pairs_df$"times_in_same_solution" <- 0
clustered_pairs_df$"times_in_same_cluster" <- 0

clustered_pairs_df |> head()

# iteration through all the clustered pairs
for (row in seq_len(nrow(clustered_pairs_df))) {
    for (subsample in seq_len(length(subsample_solutions))) {
        if clustered_pairs_df[]
    }
}

print(clustered_pairs_df[3, ])

subsample_solutions[[1]]

zz <- data.frame(
    z1 = c("hi", "dog", "cat"),
    z2 = c("bad", "cat", "dog")
)
zz$"times_in_same_solution" <- 0
zz$"times_in_same_cluster" <- 0
for (row in seq_len(nrow(zz))) {
    current_row <- zz[row, ]
    if ("dog" %in% current_row & "cat" %in% current_row) {
        zz[row, ]$"times_in_same_cluster" <- zz[row, ]$"times_in_same_cluster" + 1
        zz[row, ]$"times_in_same_solution" <- zz[row, ]$"times_in_same_solution" + 1
    }
}

zz

zz |>
    apply(
        1,
        function(x) {
            class(x)
            #if ("hi" %in% x & "bad" %in% x) {
            #    x$"times_in_same_cluster" <- x$"times_in_same_cluster" + 1
            #    x$"times_in_same_solution" <- x$"times_in_same_solution" + 1
            #}
        }
    )

#for (solutions_matrix_row in seq_len(nrow(solutions_matrix))) {
#    print(solutions_matrix_row)
#}

# solutions matrix row
# mean ari
# ari sd

# library(dbscan)

# Stability:
# - adjusted rand index across all iterations X

# - percentage of time that a patient clustered with another patient from their own cluster
# 1. find all the pairs of subjects that were in the same cluster
# 2. loop through those pairs
# 3. for each of those pairs, find all the subsamples where they were both present (both_present)
# 4. for each of those pairs, find all the subsamples where they were both in same cluster (same_cluster)
# 5. for each of those pairs, divide same_cluster/both_present (fraction_together)
# 6. calculate the mean across those fractions (mean_fraction_together)
