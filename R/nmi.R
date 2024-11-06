#' Calculate feature NMIs for a data_list and a derived solutions_matrix
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#' Use the same value as was used in the original call to `batch_snf()`.
#'
#' @param solutions_matrix Result of `batch_snf` storing cluster solutions and
#' the settings that were used to generate them. Use the same value as was used
#' in the original call to `batch_snf()`.
#'
#' @param clust_algs_list List of custom clustering algorithms to apply
#' to the final fused network. See ?generate_clust_algs_list. Use the same
#' value as was used in the original call to `batch_snf()`.
#'
#' @param distance_metrics_list An optional nested list containing which
#' distance metric function should be used for the various feature types
#' (continuous, discrete, ordinal, categorical, and mixed). Use the same value
#' as was used in the original call to `batch_snf()`.
#'
#' @param automatic_standard_normalize If TRUE, will automatically apply
#' standard normalization prior to calculation of any distance matrices. Use
#' the same value as was used in the original call to `batch_snf()`.
#'
#' @param transpose If TRUE, will transpose the output dataframe.
#'
#' @param ignore_inclusions If TRUE, will ignore the inclusion columns in the
#' solutions matrix and calculate NMIs for all features. If FALSE, will give
#' NAs for features that were dropped on a given settings_matrix row.
#'
#' @param verbose If TRUE, print progress to console.
#'
#' @return A "data.frame" class object containing one row for every feature
#' in the provided data list and one column for every solution in the provided
#' solutions matrix. Populated values show the calculated NMI score for each
#' feature-solution combination.
#'
#' @export
batch_nmi <- function(data_list,
                      solutions_matrix,
                      clust_algs_list = NULL,
                      distance_metrics_list = NULL,
                      automatic_standard_normalize = FALSE,
                      transpose = TRUE,
                      ignore_inclusions = TRUE,
                      verbose = FALSE) {
    ###########################################################################
    # Dataframe storing all the features in the data list
    ###########################################################################
    dl_df <- collapse_dl(data_list)
    ###########################################################################
    # Extracting features in the data list
    ###########################################################################
    features <- colnames(dl_df[-1])
    ###########################################################################
    # Extracting settings used to generate the solutions matrix. These will be
    # used to generate the new solutions based on the solo features.
    ###########################################################################
    settings_matrix <- no_subs(solutions_matrix)
    ###########################################################################
    # nmi_df will store all the NMIs for each feature
    ###########################################################################
    nmi_df <- solutions_matrix[, "row_id", drop = FALSE]
    ###########################################################################
    # If ignore_inclusions is TRUE, all inclusion columns will be set to 1
    ###########################################################################
    if (ignore_inclusions) {
        settings_matrix <- settings_matrix |> dplyr::mutate(
            dplyr::across(
                dplyr::starts_with("inc_"), ~ 1
            )
        )
    }
    ###########################################################################
    # Loop through each feature
    ###########################################################################
    for (i in seq_along(features)) {
        feature <- features[i]
        if (verbose) {
            print(
                paste0(
                    "Calculating NMIs for ",
                    feature, " (feature ", i, "/", length(features), ")..."
                )
            )
        }
        #######################################################################
        # Reduced data list containing only the current feature
        #######################################################################
        feature_dl <- lapply(
            data_list,
            function(component) {
                if (feature %in% colnames(component$"data")) {
                    component$"data" <- component$"data" |>
                        dplyr::select(
                            dplyr::all_of(
                                c(
                                    "subjectkey",
                                    feature
                                )
                            )
                        )
                    return(component)
                }
            }
        )
        #######################################################################
        # Stripping away other inclusion columns from settings matrix
        #######################################################################
        feature_dl <- feature_dl[!sapply(feature_dl, is.null)]
        inc_this_data_type <- paste0("inc_", feature_dl[[1]]$"name")
        inc_columns <- startsWith(colnames(settings_matrix), "inc_")
        is_this_inc <- colnames(settings_matrix) == inc_this_data_type
        keep_cols <- is_this_inc | !inc_columns
        feature_settings_matrix <- settings_matrix[, keep_cols]
        #######################################################################
        # Vector storing this feature's NMIs
        #######################################################################
        feature_nmis <- c()
        #######################################################################
        # Loop through the settings matrix and run solo-feature SNFs
        #######################################################################
        for (j in seq_len(nrow(feature_settings_matrix))) {
            this_settings_matrix <- feature_settings_matrix[j, ]
            this_inclusion <- this_settings_matrix[, inc_this_data_type]
            if (!ignore_inclusions && this_inclusion == 0) {
                ###############################################################
                # If feature is dropped and inc not ignored, the NMI is NA
                ###############################################################
                feature_nmis <- c(feature_nmis, NA)
            } else {
                ###############################################################
                # Running SNF
                ###############################################################
                # Aliasing to avoiding excess column length
                asn <- automatic_standard_normalize
                this_solutions_matrix <- batch_snf(
                    data_list = feature_dl,
                    settings_matrix = this_settings_matrix,
                    clust_algs_list = clust_algs_list,
                    distance_metrics_list = distance_metrics_list,
                    automatic_standard_normalize = asn,
                    verbose = FALSE
                )
                ###############################################################
                # Inner join to ensure consistent subject order
                ###############################################################
                solo_solution <- get_cluster_df(this_solutions_matrix)
                full_solution <- get_cluster_df(solutions_matrix[j, ])
                colnames(solo_solution) <- c("subjectkey", "solo_cluster")
                joint_solution <- dplyr::inner_join(
                    solo_solution,
                    full_solution,
                    by = "subjectkey"
                )
                ###############################################################
                # Calculate NMI
                ###############################################################
                nmi <- SNFtool::calNMI(
                    joint_solution$"solo_cluster",
                    joint_solution$"cluster"
                )
                ###############################################################
                # Append to this feature's NMI vector
                ###############################################################
                feature_nmis <- c(feature_nmis, nmi)
            }
        }
        #######################################################################
        # Combine this feature's NMIs with the overall NMI dataframe
        #######################################################################
        nmi_df <- data.frame(nmi_df, new_feature = feature_nmis)
        colnames(nmi_df)[ncol(nmi_df)] <- feature
    }
    ###########################################################################
    # Transpose and clean
    ###########################################################################
    if (transpose) {
        row_ids <- nmi_df$"row_id"
        nmi_df <- t(nmi_df)
        nmi_df <- nmi_df[-1, , drop = FALSE]
        nmi_df <- data.frame(rownames(nmi_df), nmi_df)
        rownames(nmi_df) <- NULL
        colnames(nmi_df) <- c("feature", paste0("row_id_", row_ids))
        return(nmi_df)
    }
    return(nmi_df)
}
