#' Quality metrics
#'
#' These functions calculate conventional metrics of cluster solution quality.
#'
#' calculate_silhouettes: A wrapper for `cluster::silhouette` that calculates
#' silhouette scores for all cluster solutions in a provided solutions data
#' frame. Silhouette values range from -1 to +1 and indicate an overall ratio
#' of how close together observations within a cluster are to how far apart
#' observations across clusters are. You can learn more about interpreting
#' the results of this function by calling `?cluster::silhouette`.
#'
#' calculate_dunn_indices: Calculates Dunn indices for all cluster solutions in
#' a provided solutions data frame. 
#'
#' calculate_db_indices: Calculates Davies-Bouldin indices for all cluster
#' solutions in a provided solutions data frame.
#'
#' For Dunn and Davies-Bouldin index calculations, a grid of values is presented
#' based on multiple combinations of intra-cluster distance methods (complete
#' or average distance) and inter-cluster distance methods (single-linkage,
#' complete-linkage, average distance, and Hausdorff distance).
#'
#' @param sol_df A `solutions_df` class object created by `batch_snf()` with
#'  the parameter `return_sim_mats = TRUE`.
#' @return A list of `silhouette` class objects, a vector of Dunn indices, or a
#'  vector of Davies-Bouldin indices depending on which function was used.
#' @examples
#' \dontrun{
#' input_dl <- data_list(
#'     list(gender_df, "gender", "demographics", "categorical"),
#'     list(diagnosis_df, "diagnosis", "clinical", "categorical"),
#'     uid = "patient_id"
#' )
#' 
#' sc <- snf_config(input_dl, n_solutions = 5)
#' 
#' sol_df <- batch_snf(input_dl, sc, return_sim_mats = TRUE)
#' 
#' # calculate Davies-Bouldin indices
#' davies_bouldin_indices <- calculate_db_indices(sol_df)
#' 
#' # calculate Dunn indices
#' dunn_indices <- calculate_dunn_indices(sol_df)
#' 
#' # calculate silhouette scores
#' silhouette_scores <- calculate_silhouettes(sol_df)
#' }
#' @name quality_measures
NULL

#' @rdname quality_measures
#' @export
calculate_silhouettes <- function(sol_df) {
    similarity_matrices <- sim_mats_list(sol_df)
    if (all(sapply(similarity_matrices, is.null))) {
        metasnf_error(
            "Solutions data frame is missing similarity matrices attribute.",
            " Please set `return_sim_mats = TRUE` when calling `batch_snf()`."
        )
    }
    # Average out the intense signal present in the diagonals of the similarity
    #  matrices. Also, convert them into dissimilarity matrices by the logic
    #  of dissimilarity = max(similarity) - similarity.
    dissimilarity_matrices <- similarity_matrices |>
        lapply(
            function(similarity_matrix) {
                diag(similarity_matrix) <- mean(similarity_matrix)
                dissimilarity_matrix <- max(similarity_matrix) -
                    similarity_matrix
                return(dissimilarity_matrix)
            }
        )
    # Dataframe that contains patients along the rows, sol_df rows
    #  along the columns, and which cluster the patient was assigned to in the
    #  values.
    cluster_solutions_df <- t(sol_df)
    # cluster_solutions is a list of cluster solutions. Each element in the
    #  list is a column from cluster_solutions_df, excluding the uid
    #  column.
    cluster_solutions <- sapply(
        cluster_solutions_df[, -1],
        function(column) {
            list(column)
        }
    )
    silhouette_scores <- Map(
        function(cluster_solution, dissimilarity_matrix) {
            # Note: the cluster package should not be converted to an optional
            #  package in "Suggests". `cluster::daisy` is a default distance
            #  measure required for categorical and mixed data.
            silhouette_score <- cluster::silhouette(
                x = as.integer(cluster_solution),
                dmatrix = dissimilarity_matrix
            )
            return(silhouette_score)
        },
        cluster_solutions,
        dissimilarity_matrices
    )
    return(silhouette_scores)
}

#' @rdname quality_measures
#' @export
calculate_dunn_indices <- function(sol_df) {
    # avoid visible binding problems for tidyr
    s_type <- ""
    s_complete <- ""
    s_average <- ""
    dunn <- ""
    sim_mats <- attributes(sol_df)$"sim_mats_list"
    all_is_null <- lapply(
        sim_mats,
        function(x) {
            is.null(x)
        }
    ) |>
        unlist() |>
        all()
    if (all_is_null) {
        metasnf_error(
            "Solutions data frame is missing similarity matrices attribute.",
            " Please set `return_sim_mats = TRUE` when calling `batch_snf()`."
        )
    }
    # Average out the intense signal present in the diagonals of the similarity
    #  matrices. Also, convert them into dissimilarity matrices by the logic
    #  of dissimilarity = max(similarity) - similarity.
    dist_mats <- lapply(sim_mats, sim_to_dist)
    cluster_solutions_df <- t(sol_df)
    cluster_solutions <- sapply(
        cluster_solutions_df[, -1],
        function(column) {
            list(column)
        }
    )
    dunn_indices <- Map(
        function(cluster_solution, dist_mat) {
            dunn_manual(
                dist_mat = dist_mat,
                clusters = cluster_solution
            )
        },
        cluster_solutions,
        dist_mats
    )
    dunn_indices <- lapply(
        dunn_indices,
        function(df) {
            df <- df |>
                tidyr::pivot_wider(names_from = s_type, values_from = dunn) |>
                tibble::column_to_rownames("m_type") |>
                dplyr::select(s_complete, s_average) |>
                as.matrix()
            df[c("m_single", "m_complete", "m_average", "m_hausdorff"), ]
        }
    )
    return(dunn_indices)
}

#' Calculate Dunn index
#'
#' @keywords internal
#' @param dist_mat A dissimilarity matrix.
#' @param clusters An integer vector of cluster assignments for that matrix.
#' @return Dunn index (numeric).
dunn_manual <- function(dist_mat, clusters) {
    n_clusters <- max(clusters)
    # calculating intra-cluster dispersion metric s
    s_complete <- lapply(
        1:n_clusters,
        function(i) {
            ci_indices <- which(clusters == i)
            intra_dist_mat <- dist_mat[ci_indices, ci_indices]
            max(intra_dist_mat)
        }
    )
    s_average <- lapply(
        1:n_clusters,
        function(i) {
            ci_indices <- which(clusters == i)
            intra_dist_mat <- dist_mat[ci_indices, ci_indices]
            mean(intra_dist_mat[upper.tri(intra_dist_mat)])
        }
    )
    # calculating inter-cluster characteristic metric m
    m_complete   <- matrix(NA, n_clusters, n_clusters)
    m_single     <- matrix(NA, n_clusters, n_clusters)
    m_average    <- matrix(NA, n_clusters, n_clusters)
    m_hausdorff  <- matrix(NA, n_clusters, n_clusters)
    for (i in 1:n_clusters) {
        for (j in 1:n_clusters) {
            if (i != j) {
                ci_indices <- which(clusters == i)
                cj_indices <- which(clusters == j)
                comparison_dist_mat <- dist_mat[ci_indices, cj_indices]
                m_complete[i, j]  <- max(comparison_dist_mat)  
                m_single[i, j]    <- min(comparison_dist_mat)  
                m_average[i, j]   <- mean(comparison_dist_mat)
                m_hausdorff[i, j] <- max(
                    max(apply(comparison_dist_mat, 1, min)),
                    max(apply(comparison_dist_mat, 2, min))
                )
            }
        }
    }
    s_vectors <- list(
        "s_complete" = unlist(s_complete),
        "s_average" = unlist(s_average)
    )
    m_distances <- list(
        "m_complete" = m_complete,
        "m_average" = m_average,
        "m_single" = m_single,
        "m_hausdorff" = m_hausdorff
    )
    dunn_df <- data.frame()
    for (s_type in names(s_vectors)) {
        for (m_type in names(m_distances)) {
            s_vector <- s_vectors[[s_type]]
            m_dist <- m_distances[[m_type]]
            dunn <- min(m_dist, na.rm = TRUE) / max(s_vector)
            dunn_df <- rbind(
                dunn_df, 
                data.frame(
                    s_type = s_type,
                    m_type = m_type,
                    dunn = dunn
                )
            )
        }
    }
    dunn_df
}

#' @rdname quality_measures
#' @export
calculate_db_indices <- function(sol_df) {
    # avoid visible binding problems for tidyr
    s_type <- ""
    s_complete <- ""
    s_average <- ""
    r <- ""
    # 1. Generate dissimilarity matrix
    sim_mats <- attributes(sol_df)$"sim_mats_list"
    all_is_null <- lapply(
        sim_mats,
        function(x) {
            is.null(x)
        }
    ) |>
        unlist() |>
        all()
    if (all_is_null) {
        metasnf_error(
            "Solutions data frame is missing similarity matrices attribute.",
            " Please set `return_sim_mats = TRUE` when calling `batch_snf()`."
        )
    }
    # Average out the intense signal present in the diagonals of the similarity
    #  matrices. Also, convert them into dissimilarity matrices by the logic
    #  of dissimilarity = max(similarity) - similarity.
    dist_mats <- lapply(sim_mats, sim_to_dist)
    cluster_solutions_df <- t(sol_df)
    cluster_solutions <- sapply(
        cluster_solutions_df[, -1],
        function(column) {
            list(column)
        }
    )
    db_indices <- Map(
        function(cluster_solution, dist_mat) {
            cluster_solution <- as.integer(cluster_solution)
            db_index <- db_manual(
                as.matrix(dist_mat),
                cluster_solution
            )
            return(db_index)
        },
        cluster_solutions,
        dist_mats
    )
    db_indices <- lapply(
        db_indices,
        function(df) {
            df <- df |>
                tidyr::pivot_wider(names_from = s_type, values_from = r) |>
                tibble::column_to_rownames("m_type") |>
                dplyr::select(s_complete, s_average) |>
                as.matrix()
            df[c("m_single", "m_complete", "m_average", "m_hausdorff"), ]
        }
    )
    return(db_indices)
}

#' Calculate Davies-Bouldin index
#'
#' @keywords internal
#' @param dist_mat A dissimilarity matrix.
#' @param clusters An integer vector of cluster assignments for that matrix.
#' @return Davies-Bouldin index (numeric).
db_manual <- function(dist_mat, clusters) {
    # avoid visible binding problems for tidyr
    r_i <- ""
    n_clusters <- max(clusters)
    # calculating intra-cluster dispersion metric s
    s_complete <- lapply(
        1:n_clusters,
        function(i) {
            ci_indices <- which(clusters == i)
            intra_dist_mat <- dist_mat[ci_indices, ci_indices]
            max(intra_dist_mat)
        }
    )
    s_average <- lapply(
        1:n_clusters,
        function(i) {
            ci_indices <- which(clusters == i)
            intra_dist_mat <- dist_mat[ci_indices, ci_indices]
            mean(intra_dist_mat[upper.tri(intra_dist_mat)])
        }
    )
    # calculating inter-cluster characteristic metric m
    m_complete   <- matrix(NA, n_clusters, n_clusters)
    m_single     <- matrix(NA, n_clusters, n_clusters)
    m_average    <- matrix(NA, n_clusters, n_clusters)
    m_hausdorff  <- matrix(NA, n_clusters, n_clusters)
    for (i in 1:n_clusters) {
        for (j in 1:n_clusters) {
            if (i != j) {
                ci_indices <- which(clusters == i)
                cj_indices <- which(clusters == j)
                comparison_dist_mat <- dist_mat[ci_indices, cj_indices]
                m_complete[i, j]  <- max(comparison_dist_mat)  
                m_single[i, j]    <- min(comparison_dist_mat)  
                m_average[i, j]   <- mean(comparison_dist_mat)
                m_hausdorff[i, j] <- max(
                    max(apply(comparison_dist_mat, 1, min)),
                    max(apply(comparison_dist_mat, 2, min))
                )
            }
        }
    }
    s_vectors <- list(
        "s_complete" = unlist(s_complete),
        "s_average" = unlist(s_average)
    )
    m_distances <- list(
        "m_complete" = m_complete,
        "m_average" = m_average,
        "m_single" = m_single,
        "m_hausdorff" = m_hausdorff
    )
    r_df <- data.frame()
    for (i in 1:n_clusters) {
        for (j in 1:n_clusters) {
            if (i != j) {
                for (s_type in names(s_vectors)) {
                    for (m_type in names(m_distances)) {
                        s_vector <- s_vectors[[s_type]]
                        m_dist <- m_distances[[m_type]]
                        r_ij <- (s_vector[[i]] + s_vector[[j]]) / m_dist[i, j]
                        r_df <- rbind(
                            r_df, 
                            data.frame(
                                s_type = s_type,
                                m_type = m_type,
                                r_ij = r_ij,
                                i = i
                            )
                        )
                    }
                }
            }
        }
    }
    r_df_final <- r_df |>
        dplyr::summarize(r_i = max(r_ij), .by = c(s_type, m_type, i)) |>
        dplyr::summarize(r = mean(r_i), .by = c(s_type, m_type))
    r_df_final
}


#' Convert a similarity matrix to a dissimilarity matrix
#'
#' @keywords internal
#' @param sim_mat A similarity matrix
#' @return A dissimilarity matrix
sim_to_dist <- function(sim_mat) {
    diag(sim_mat) <- mean(sim_mat)
    dist_mat <- max(sim_mat) - sim_mat
    return(dist_mat)
}
