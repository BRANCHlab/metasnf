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

# Should save many affinity matrices to disk
# output_matrix <- batch_snf(data_list, settings_matrix, affinity_matrix_dir = ".")

# Should error
# output_matrix <- batch_snf(data_list, settings_matrix, run_clustering = FALSE)

# Should eventually complete
# output_matrix <- batch_snf(data_list, settings_matrix, processes = "max")

available_cores <- future::availableCores()[["cgroups.cpuset"]]

future::availableCores()[["system"]]


colnames(output_matrix)

colnames(no_subs(output_matrix))

write.csv(output_matrix, "mymatrix.csv", row.names = TRUE)
om_read <- read.csv("mymatrix.csv", row.names = 1)

i <- 10
affinity_matrix_dir <- "this/is/the-path"


# Clustering functions should take in:
#   1. an affinity matrix
# and return a list of the following:
#   2. the cluster solution
spectral_eigen <- function(affinity_matrix) {
    estimated_n <- SNFtool::estimateNumberOfClustersGivenGraph(affinity_matrix)
    number_of_clusters <- estimated_n$`Eigen-gap best`
    solution <- SNFtool::spectralClustering(fused_network, number_of_clusters)
    return(solution)
}

spectral_rot <- function(affinity_matrix) {
    estimated_n <- SNFtool::estimateNumberOfClustersGivenGraph(affinity_matrix)
    number_of_clusters <- estimated_n$`Rotation cost best`
    solution <- SNFtool::spectralClustering(fused_network, number_of_clusters)
    return(solution)
}

clustering_algs <- list(
)


colnames(settings_matrix)
