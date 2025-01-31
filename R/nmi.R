#' Calculate feature NMIs for a data list and a solutions data frame
#'
#' Normalized mutual information scores can be used to indirectly measure how
#' important a feature may have been in producing a cluster solution. This
#' function will calculate the normalized mutual information between cluster
#' solutions in a solutions data frame as well as cluster solutions created
#' by including only a single feature from a provided data list, but otherwise
#' using all the same hyperparameters as specified in the original SNF config.
#' Note that NMIs can be calculated between two cluster solutions regardless
#' of what features were actually used to create those cluster solutions. For
#' example, a feature that was not involved in producing a particular cluster
#' solution may still have a high NMI with that cluster solution (typically
#' because it was highly correlated with a different feature that was used).
#'
#' @inheritParams batch_snf
#' @param sol_df Result of `batch_snf` storing cluster solutions and
#'  the settings that were used to generate them. Use the same value as was used
#'  in the original call to `batch_snf()`.
#' @param transpose If TRUE, will transpose the output data frame.
#' @param ignore_inclusions If TRUE, will ignore the inclusion columns in the
#'  solutions data frame and calculate NMIs for all features. If FALSE, will
#'  give NAs for features that were dropped on a given settings_df row.
#' @param verbose If TRUE, output progress to console.
#' @return A "data.frame" class object containing one row for every feature
#'  in the provided data list and one column for every solution in the provided
#'  solutions data frame. Populated values show the calculated NMI score for
#'  each feature-solution combination.
#' @export
#' @examples
#' input_dl <- data_list(
#'     list(gender_df, "gender", "demographics", "categorical"),
#'     list(diagnosis_df, "diagnosis", "clinical", "categorical"),
#'     uid = "patient_id"
#' )
#' 
#' sc <- snf_config(input_dl, n_solutions = 2)
#' 
#' sol_df <- batch_snf(input_dl, sc)
#' 
#' calc_nmis(input_dl, sol_df)
calc_nmis <- function(dl,
                      sol_df,
                      transpose = TRUE,
                      ignore_inclusions = TRUE,
                      verbose = FALSE) {
    # A vector of features from the data list
    features <- attributes(dl)$"features"
    # An `snf_config` object used to create sol_df
    sc <- attributes(sol_df)$"snf_config"
    # A data frame with the same nrows as sol_df and column name "solution"
    nmi_df <- sol_df[, "solution", drop = FALSE]
    ###########################################################################
    # If ignore_inclusions is TRUE, all inclusion columns will be set to 1
    if (ignore_inclusions) {
        sc$"settings_df" <- sc$"settings_df" |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::starts_with("inc_"), ~ 1
                )
            )
    }
    ###########################################################################
    # Loop through each feature
    for (i in seq_along(features)) {
        feature <- features[i]
        if (verbose) {
            cat(
                "Calculating NMI for ",
                feature, " (feature ", i, "/", length(features), ")...\n",
                sep = ""
            )
        }
        #######################################################################
        # Reduced data list containing only the current feature
        feature_dl <- lapply(
            dl,
            function(component) {
                if (feature %in% colnames(component$"data")) {
                    component$"data" <- component$"data"[, c("uid", feature)]
                    return(component)
                }
            }
        )
        #######################################################################
        # Stripping away other inclusion columns
        feature_dl <- feature_dl[!sapply(feature_dl, is.null)]
        feature_dl <- as_data_list(feature_dl)
        inc_this_data_type <- paste0("inc_", feature_dl[[1]]$"name")
        inc_columns <- startsWith(colnames(sc$"settings_df"), "inc_")
        is_this_inc <- colnames(sc$"settings_df") == inc_this_data_type
        keep_cols <- is_this_inc | !inc_columns
        feature_settings_df <- sc$"settings_df"[, keep_cols]
        #######################################################################
        # Vector storing this feature's NMIs
        feature_nmis <- c()
        #######################################################################
        # Loop through the settings data frame and run solo-feature SNFs
        for (j in seq_len(nrow(feature_settings_df))) {
            this_sc <- sc[j]
            this_inclusion <- this_sc$"settings_df"[, inc_this_data_type]
            if (!ignore_inclusions && this_inclusion == 0) {
                # If feature is dropped and inc not ignored, the NMI is NA
                feature_nmis <- c(feature_nmis, NA)
            } else {
                # Running SNF
                this_sol_df <- batch_snf(
                    dl = feature_dl,
                    sc = this_sc
                )
                # Inner join to ensure consistent subject order
                solo_solution <- t(this_sol_df)
                full_solution <- t(sol_df[j, ])
                colnames(solo_solution) <- c("uid", "solo_cluster")
                joint_solution <- dplyr::inner_join(
                    solo_solution,
                    full_solution,
                    by = "uid"
                )
                # Calculate NMI
                nmi <- SNFtool::calNMI(
                    joint_solution$"solo_cluster",
                    joint_solution[, 3]
                )
                # Append to this feature's NMI vector
                feature_nmis <- c(feature_nmis, nmi)
            }
        }
        #######################################################################
        # Combine this feature's NMIs with the overall NMI data frame
        nmi_df <- data.frame(nmi_df, new_feature = feature_nmis)
        colnames(nmi_df)[ncol(nmi_df)] <- feature
    }
    ###########################################################################
    # Transpose and clean
    ###########################################################################
    if (transpose) {
        solution_idx <- nmi_df$"solution"
        nmi_df <- t(nmi_df)
        nmi_df <- nmi_df[-1, , drop = FALSE]
        nmi_df <- data.frame(rownames(nmi_df), nmi_df)
        rownames(nmi_df) <- NULL
        colnames(nmi_df) <- c("feature", paste0("s", solution_idx))
        return(nmi_df)
    }
    return(nmi_df)
}
