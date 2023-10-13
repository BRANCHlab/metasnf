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

silhouette <- function(solutions_matrix, affinity_matrices) {
    print(5)
}

library(cluster)
library(pheatmap)

pheatmap(am1)

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

diag(am1) <- 0

print(max(am1))
am1 <- max(am1) - am1
cluster_codes <- get_cluster_df(solutions_matrix[1, ])$"cluster"
z <- silhouette(
    x = cluster_codes,
    dmatrix = am1
)
plot(z)


data.frame(z)


class(am1)


get_clusters(solutions_matrix)

qq <- solutions_matrix |>
    subs() |>
    t() |>
    data.frame()

colnames(qq) <- rownames(solutions_matrix)

qq <- qq[-1, ]

head(qq)

subby <- data.frame(rownames(qq))

colnames(subby) <- "subjectkey"

dim(subby)

dim(qq)

qq2 <- cbind(subby, qq)

rownames(qq2) <- NULL

qq2

rownames(solutions_matrix)

ncol(solutions_matrix)

subs(solutions_matrix)

get_cluster_solutions(solutions_matrix)
