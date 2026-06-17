devtools::load_all()
#library(metasnf)
dl <- data_list(
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    list(depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)
# The default list:
sc <- snf_config(
    dl = dl,
    n_solutions = 5,
    use_default_clust_fns = TRUE
)
input_dl <- data_list(
    list(gender_df, "gender", "demographics", "categorical"),
    list(diagnosis_df, "diagnosis", "clinical", "categorical"),
    uid = "patient_id"
)
sc <- snf_config(input_dl, n_solutions = 2)
sol_df <- batch_snf(input_dl, sc, return_sim_mats = TRUE)

## calculate Dunn indices
calculate_dunn_indices(sol_df)

calculate_db_indices(sol_df)

calculate_silhouettes(sol_df)


function (distance = NULL, clusters, Data = NULL, method = "euclidean") {
    if (is.null(distance) & is.null(Data))
        stop("One of 'distance' or 'Data' is required")
    if (is.null(distance))
        distance <- as.matrix(dist(Data, method = method))
    if ("dist" %in% class(distance))
        distance <- as.matrix(distance)
    nc <- max(clusters)
    interClust <- matrix(NA, nc, nc)
    intraClust <- rep(NA, nc)
    for (i in 1:nc) {
        c1 <- which(clusters == i)
        for (j in i:nc) {
            if (j == i)
                intraClust[i] <- max(distance[c1, c1])
            if (j > i) {
                c2 <- which(clusters == j)
                interClust[i, j] <- min(distance[c1, c2])
            }
        }
    }
    dunn <- min(interClust, na.rm = TRUE)/max(intraClust)
    return(dunn)
}
