library(metasnf)
data_list <- generate_data_list(
    list(abcd_cort_t, "cortical_thickness", "neuroimaging", "continuous"),
    list(abcd_cort_sa, "cortical_surface_area", "neuroimaging", "continuous"),
    list(abcd_subc_v, "subcortical_volume", "neuroimaging", "continuous"),
    list(abcd_income, "household_income", "demographics", "continuous"),
    list(abcd_pubertal, "pubertal_status", "demographics", "continuous"),
    uid = "patient"
)

settings_matrix <- generate_settings_matrix(
    data_list,
    nrow = 5,
    max_k = 40,
    seed = 42
)

batch_snf(
    data_list,
    settings_matrix
)

summarize_dl(data_list)

dl <- domain_merge(data_list)

summarize_dl(dl)

data_list |>
    lapply(
        function(x) {
            x$"domain" == "neuroimaging"
        }
    ) |> unlist() |> any()

merged_dl <- list()

"neuroimaging" %in% summarize_dl(data_list)$"domain"

summarize_dl(data_list)

which(summarize_dl(data_list)$"domain" == "neuroimaging")



z <- domain_merge(data_list)

z

z <- domain_merge(data_list)

summarize_dl(z)

domain_merge <- function(data_list) {
    # create a list to store the final result
    domain_dl <- list()
    # iterate through the list
    for (i in seq_along(data_list)) {
        # the entire current component
        current_component <- data_list[[i]]
        # the current domain
        current_domain <- data_list[[i]]$"domain"
        if (length(domain_dl) == 0) {
            domain_dl <- append(domain_dl, list(current_component))
            print(domains(domain_dl))
            existing_match_pos <- which(domains(domain_dl) == current_domain)
            existing_component <- domain_dl[[existing_match_pos]]
            existing_match_data <- existing_component$"data"
            data_to_merge <- current_component$"data"
            merged_data <- dplyr::inner_join(
                existing_match_data, data_to_merge, by = "subjectkey")
            merged_component <- existing_component
            merged_component$"data" <- merged_data
            merged_component$"name" <-
                paste0("merged_", merged_component$"domain")
            merged_component$"type" <- dplyr::case_when(
                existing_component$"type" == current_component$"type" ~
                    current_component$"type",
                existing_component$"type" != current_component$"type" ~
                    "mixed"
            )
            domain_dl[[existing_match_pos]] <- merged_component
        } else {
            domain_dl <- append(domain_dl, list(current_component))
        }
    }
    return(domain_dl)
}

















mock_data <- data.frame(
    col1 = c(1, 4, 7),
    col2 = c(1, 5, 9)
) |> as.matrix()

mock_data

weighted_euclidean_distance(mock_data, weights_df)

weighted_euclidean_distance(mock_data, data.frame(c(1, 1)))

weights_df <- data.frame(
    weights = c(1, 2)
)

weights_df

mock_data

wt_sq_euclidean_distance <- function(df, weights) {
}


weights_matrix <- generate_weights_matrix(
    data = mock_data,
    rows = 10,
    fill = "uniform"
)

# May need a left or right join to properly reduce the weights df row



#' Distance metric: Weighted Euclidean distance
#'
#' @param df Dataframe containing one subjectkey column in the first column and
#'  at least 1 continuous data column. All feature data should be continuous.
#' @param weights Dataframe with 1 column containing weights for each feature per
#'  row in the same order as the order of feature columns start
#'
#' @return weighted_distance_matrix A distance matrix.
#'
#' @export
weighted_euclidean_distance <- function(df, weights) {
    if (!requireNamespace("abSNF", quietly = TRUE)) {
        stop(
            "Package \"abSNF\" must be installed to use this function.",
            call. = FALSE
        )
    }
    weights_mat <- data.matrix(weights)
    weighted_dist <- abSNF::dist2_w(
        X = df,
        C = df,
        weight = weights_mat
    )
    return(weighted_dist)
}
