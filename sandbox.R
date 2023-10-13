library(metasnf)

data_list <- generate_data_list(
    list(abcd_cort_t, "cortical_thickness", "neuroimaging", "numeric"),
    list(abcd_cort_sa, "cortical_surface_area", "neuroimaging", "numeric"),
    list(abcd_subc_v, "subcortical_volume", "neuroimaging", "numeric"),
    list(abcd_income, "household_income", "demographics", "numeric"),
    list(abcd_pubertal, "pubertal_status", "demographics", "numeric"),
    old_uid = "patient"
)

settings_matrix <- generate_settings_matrix(data_list, nrow = 15, seed = 42)


solutions_matrix <- batch_snf(data_list, settings_matrix)

batch_snf_results <- batch_snf(
    data_list,
    settings_matrix,
    return_affinity_matrices = TRUE
)

solutions_matrix <- batch_snf_results$"solutions_matrix"
affinity_matrices <- batch_snf_results$"affinity_matrices"


library(cluster)
library(pheatmap)

calculate_silhouettes(solutions_matrix, affinity_matrices)

am1 <- affinity_matrices[[1]]
diag(am1) <- mean(am1)

am1 <- max(am1) - am1


cluster_codes <- get_cluster_df(solutions_matrix[1, ])$"cluster"

z <- silhouette(
    x = cluster_codes,
    dmatrix = am1
)

plot(z)

am1 <- affinity_matrices[[1]]

print(max(am1))
am1 <- max(am1) - am1
cluster_codes <- get_cluster_df(solutions_matrix[1, ])$"cluster"
z <- silhouette(
    x = cluster_codes,
    dmatrix = am1
)
plot(z)


data.frame(z)


cluster_solutions <- get_cluster_solutions(solutions_matrix)

silhouette(
    x = cluster_solutions[, 2],
    dmatrix = am1
) |> plot()


z <- list(1, 2, 3, 4)

q <- list(3, 10, 100, 1000)

Map(
    function(x, y) {
        return(x + y)
    },
    z,
    q
)

cluster_solutions

for (i in seq_len(ncol(cluster_solutions[, -1]))) {
    print(i)
}
