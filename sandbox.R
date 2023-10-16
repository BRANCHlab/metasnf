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

solutions_matrix |> no_subs()

data_list

summarize_dl(data_list)

sample_data_list <- function(data_list,
                             n_samples,
                             sample_fraction = NULL,
                             n_subjects = NULL) {
    # Make sure that only one parameter was used to specify how many subjects
    #  to keep in each subsample
    both_null <- is.null(sample_fraction) & is.null(n_subjects)
    neither_null <- !is.null(sample_fraction) & !is.null(n_subjects)
    if (both_null | neither_null) {
        stop(
            paste0(
                "Either the sample_fraction parameter (fraction of subjects)",
                " or n_subjects (number of subjects) must be provided. Not",
                " both (or neither)."
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
    if (!is.null(sample_fraction)) {
        if (sample_fraction > 1 | sample_fraction < 0) {
            stop(
                "sample_fraction must be between 0 and 1."
            )
        } else {
            n_subjects <- round(sample_fraction * length(all_subjects))
        }
    }
    subject_subsamples <- lapply(
        rep(n_subjects, n_samples),
        function(x) {
            return(sample(all_subjects, x))
        }
    )
    data_list_subsamples <- subject_subsamples |> lapply(
        function(subsample) {
            length(subsample)
            dl_subsample <- data_list |> lapply(
                function(x) {
                    x$"data" <- x$"data"[x$"data"$"subjectkey" %in% subsample, ]
                    return(x)
                }
            )
        }
    )
    subsample_names <- paste0("subsample_", 1:n_samples)
    names(data_list_subsamples) <- subsample_names
    return(data_list_subsamples)
}

z <- sample_data_list(
    data_list,
    n_samples = 3,
    sample_fraction = 0.8
)

z$"subsample_1"


zz <- lapply(
    z,
    function(x) {
        batch_snf(
            data_list = x,
            settings_matrix,
            return_affinity_matrices = TRUE
        )
    }
)

zz[[1]]$"affinity_matrices"

zz[[1]]$"solutions_matrix"

settings_matrix$"k"
