devtools::load_all()
set.seed(44)
library(sloop)
library(testthat)

#' input_dl <- data_list(
#'     list(gender_df, "gender", "demographics", "categorical"),
#'     list(diagnosis_df, "diagnosis", "clinical", "categorical"),
#'     uid = "patient_id"
#' )
#' 
#' sc <- snf_config(input_dl, n_solutions = 1)
#' 
#' sol_df <- batch_snf(input_dl, sc, return_sim_mats = TRUE)
#' 
#' clust_fn_sequence <- list(spectral_two, spectral_four)
#' 
#' alluvial_cluster_plot(
#'     cluster_sequence = clust_fn_sequence,
#'     similarity_matrix = attributes(sol_df)$"sim_mats_list"[[1]],
#'     dl = input_dl,
#'     key_outcome = "gender", # the name of the feature of interest
#'     key_label = "Gender", # how the feature of interest should be displayed
#'     extra_outcomes = "diagnosis", # more features to plot but not colour by
#'     title = "Gender Across Cluster Counts"
#' )
