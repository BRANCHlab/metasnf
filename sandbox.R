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

sc <- snf_config(my_dl, n_solutions = 5, max_k = 40)

sol_df <- batch_snf(my_dl, sc)

my_dl_subsamples <- subsample_dl(
    my_dl,
    n_subsamples = 20,
    subsample_fraction = 0.85
)

batch_subsample_results <- batch_snf_subsamples(
    my_dl_subsamples,
    sc,
    verbose = TRUE
)

pairwise_aris <- subsample_pairwise_aris(
    batch_subsample_results,
    verbose = TRUE
)

pairwise_aris

pairwise_aris2 <- subsample_pairwise_aris(
    batch_subsample_results,
    return_raw_aris = TRUE,
    verbose = TRUE
)
print(Sys.time() - start)

coclustering_results <- calculate_coclustering(
    batch_subsample_results,
    sol_df,
    verbose = TRUE
)

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

subsample_pairwise_aris <- function(subsample_solutions, return_raw_aris = FALSE, verbose = FALSE) {
    if (length(subsample_solutions) < 3) {
        metasnf_warning("Fewer than 3 subsamples have been provided. Standard", " deviation of the pairwise ARIs for each solution", " will not be computed.")
    }
    pairwise_ari_df <- data.table("solution" = integer(), "mean_ari" = double(), "ari_sd" = double())
    pairwise_indices <- utils::combn(length(subsample_solutions), 2)
    subsample_ari_mats <- vector("list", length = nrow(subsample_solutions[[1]]))
    nrows <- nrow(subsample_solutions[[1]])
    for (row in seq_len(nrows)) {
        subsample_ari_mat <- matrix(nrow = length(subsample_solutions), ncol = length(subsample_solutions))
        colnames(subsample_ari_mat) <- paste0("subsample_", seq_len(length(subsample_solutions)))
        rownames(subsample_ari_mat) <- paste0("subsample_", seq_len(length(subsample_solutions)))
        if (verbose) {
            cat("Calculating pairwise ARIs for solution ", row, "/", nrows, "...\n", sep = "")
        }
        row_adjusted_rand_indices <- numeric(ncol(pairwise_indices))
        for (col in seq_len(ncol(pairwise_indices))) {
            v1 <- pairwise_indices[1, col]
            v2 <- pairwise_indices[2, col]
            subsample_a <- subsample_solutions[[v1]]
            subsample_b <- subsample_solutions[[v2]]
            solution_a <- subsample_a[, c(1, row + 1)]
            solution_b <- subsample_b[, c(1, row + 1)]
            setDT(solution_a)
            setDT(solution_b)
            browser()
            common_df <- merge(solution_a, solution_b, by = "uid", all = FALSE)
            ari <- mclust::adjustedRandIndex(common_df[[2]], common_df[[3]])
            subsample_ari_mat[v1, v2] <- ari
            row_adjusted_rand_indices[col] <- ari
        }
        subsample_ari_mat[lower.tri(subsample_ari_mat)] <- t(subsample_ari_mat)[lower.tri(subsample_ari_mat)]
        row_df <- data.table("solution" = row, "mean_ari" = mean(row_adjusted_rand_indices), "ari_sd" = sd(row_adjusted_rand_indices))
        pairwise_ari_df <- rbind(pairwise_ari_df, row_df)
        diag(subsample_ari_mat) <- 1
        if (return_raw_aris) {
            subsample_ari_mats[[row]] <- subsample_ari_mat
        }
    }
    if (return_raw_aris) {
        names(subsample_ari_mats) <- paste0("s", seq_len(nrows))
        results <- list("ari_summary" = pairwise_ari_df, "raw_aris" = subsample_ari_mats)
    } else {
        results <- pairwise_ari_df
    }
    return(results)
}

