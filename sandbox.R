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


batch_snf_results <- batch_snf(
    data_list,
    settings_matrix,
    suppress_clustering = TRUE,
    return_affinity_matrices = TRUE
)

solutions_matrix <- batch_snf_results$"solutions_matrix"
affinity_matrices <- batch_snf_results$"affinity_matrices"

subsample_data_list <- function(data_list,
                                n_subsamples,
                                subsample_fraction = NULL,
                                n_subjects = NULL) {
    # Make sure that only one parameter was used to specify how many subjects
    #  to keep in each subsample
    both_null <- is.null(subsample_fraction) & is.null(n_subjects)
    neither_null <- !is.null(subsample_fraction) & !is.null(n_subjects)
    if (both_null | neither_null) {
        stop(
            paste0(
                "Either the subsample_fraction parameter (fraction of",
                " subjects) or n_subjects (number of subjects) must be",
                " provided. Not both (or neither)."
            )
        )
    }
    # Calculate number of subjects to keep if fraction parameter was used
    all_subjects <- data_list[[1]]$"data"$"subjectkey"
    # Ensure n_subjects is within 0 and the total number of subjects
    if (!is.null(n_subjects)) {
        if (n_subjects < 0 | n_subjects > length(all_subjects)) {
            stop(
                paste0(
                    "n_subjects must be between 0 and the total number of",
                    " subjects."
                )
            )
        } else if (as.integer(n_subjects) != n_subjects) {
            stop(
                "n_subjects must be an integer."
            )
        }
    }
    # Ensure sample fraction is a real fraction
    if (!is.null(subsample_fraction)) {
        if (subsample_fraction > 1 | subsample_fraction < 0) {
            stop(
                "subsample_fraction must be between 0 and 1."
            )
        } else {
            n_subjects <- round(subsample_fraction * length(all_subjects))
        }
    }
    subject_subsamples <- lapply(
        rep(n_subjects, n_subsamples),
        function(x) {
            return(sample(all_subjects, x))
        }
    )
    data_list_subsamples <- subject_subsamples |> lapply(
        function(subsample) {
            length(subsample)
            dl_subsample <- data_list |> lapply(
                function(x) {
                    chosen_rows <- x$"data"$"subjectkey" %in% subsample
                    x$"data" <- x$"data"[chosen_rows, ]
                    return(x)
                }
            )
        }
    )
    subsample_names <- paste0("subsample_", 1:n_subsamples)
    names(data_list_subsamples) <- subsample_names
    return(data_list_subsamples)
}

data_list_subsamples <- subsample_data_list(
    data_list,
    n_subsamples = 3,
    subsample_fraction = 0.8
)

subsample_solutions <- lapply(
    data_list_subsamples,
    function(x) {
        solutions_matrix <- batch_snf(
            data_list = x,
            settings_matrix
        )
        cluster_solutions <- get_cluster_solutions(solutions_matrix)
        return(cluster_solutions)
    }
)

subsample_solutions[[3]]

zz <- subsample_solutions

zz[[1]]

zz |>
    lapply(
        function(x) {
            get_cluster_solutions(x)
        }
    )

zz[[1]]

mclust::adjustedRandIndex

ari <- function (x, y) {
    x <- as.vector(x)
    y <- as.vector(y)
    if (length(x) != length(y))
        stop("arguments must be vectors of the same length")
    tab <- table(x, y)
    if (all(dim(tab) == c(1, 1)))
        return(1)
    a <- sum(choose(tab, 2))
    b <- sum(choose(rowSums(tab), 2)) - a
    c <- sum(choose(colSums(tab), 2)) - a
    d <- choose(sum(tab), 2) - a - b - c
    ARI <- (a - (a + b) * (a + c)/(a + b + c + d))/((a + b + a + c)/2 - (a + b) * (a + c)/(a + b + c + d))
    return(ARI)
}


ari(c(1, 1, 1, 1, 2, 2, 2), c(4, 4, 4, 5, 6, 6, 6))

x <- c(1, 1, 1, 5, 2, 2, 2)
y <- c(4, 4, 4, 6, 6, 6, 6)

tab <- table(x, y)

tab

choose(tab, 2)



