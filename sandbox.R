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

batch_snf_results <- batch_snf(
    data_list,
    settings_matrix,
    return_affinity_matrices = TRUE
)

solutions_matrix <- batch_snf_results$"solutions_matrix"
affinity_matrices <- batch_snf_results$"affinity_matrices"


library(cluster)
library(pheatmap)

silhouette_scores <- calculate_silhouettes(solutions_matrix, affinity_matrices)

dunn_indices <- calculate_dunn_indices(solutions_matrix, affinity_matrices)

plot(silhouette_scores[[8]])

affinity_matrices

library(clv)

# load and prepare data
library(clv)
data(iris)
iris.data <- iris[,1:4]

# cluster data
agnes.mod <- agnes(iris.data) # create cluster tree
v.pred <- as.integer(cutree(agnes.mod,5)) # "cut" the tree


davies1 <- clv.Davies.Bouldin(cls.scatt, intraclust, interclust)

# 2. functional solution:

# define new Dunn and Davies.Bouldin functions
Dunn <- function(data,clust)
    clv.Dunn( cls.scatt.data(data,clust),
        intracls = c("complete","average","centroid"),
        intercls = c("single", "complete", "average","centroid", "aveToCent", "hausdorff")
    )
Davies.Bouldin <- function(data,clust)
    clv.Davies.Bouldin(cls.scatt.data(data,clust),
        intracls = c("complete","average","centroid"),
        intercls = c("single", "complete", "average","centroid", "aveToCent", "hausdorff")
    )

# compute indicies
dunn2 <- Dunn(iris.data, v.pred)
davies2 <- Davies.Bouldin(iris.data, v.pred)

affinity_matrices



# The Dunn Index is the ratio of the smallest distance between observations NOT in the same cluster to the largest intra-cluster distance

# Calculate the maximum distance within each cluster


# Calculate the

library(clv)
library(pheatmap)

data(mouse)
express <- mouse[1:25,c("M1","M2","M3","NC1","NC2","NC3")]
rownames(express) <- mouse$ID[1:25]
## hierarchical clustering
Dist <- dist(express,method="euclidean")

as.matrix(Dist) |> pheatmap()

clusterObj <- hclust(Dist, method="average")
nc <- 2 ## number of clusters
cluster <- cutree(clusterObj,nc)
dunn(Dist, cluster)


am1 <- affinity_matrices[[1]]
sol1 <- solutions_matrix[1, ] |> get_cluster_solutions()


diag(am1) <- mean(am1)
dm1 <- max(am1) - am1

dm1 |> pheatmap()

###

dm1

cluster <- sol1$`1`

cluster |> length()

dm1 |> dim()

# 1, 2, 3
cluster_labels <- unique(cluster) |> sort()

# Intra-cluster distances
# -


clv.Dunn

as.integer(cluster)

index_list <- cls.scatt.diss.mx(dm1, as.integer(cluster))


clv::clv.Dunn(
    index_list,
    intracls = c(
        "complete",
        "average"
    ),
    intercls = c(
        "single",
        "complete",
        "average",
        "hausdorff"
    )
)



