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
                      processes = 1) {
    dl_df <- as.data.frame(dl)
    dl_df$"uid" <- gsub("uid_", "", dl_df$"uid")
    dl_ft_summary <- summary(dl, "feature")
    dl_summary <- summary(dl)
    df_names <- rep(dl_summary$"name", dl_summary$"width")
    type_lookup <- stats::setNames(dl_ft_summary$"type", dl_ft_summary$"name")
    name_lookup <- stats::setNames(df_names, dl_ft_summary$"name")
    sc <- attr(sol_df, "snf_config")
    if (ignore_inclusions) {
        # Turn off feature inclusion/exclusion (drop no data)
        sc$"settings_df"[, grepl("inc_", colnames(sc$"settings_df"))] <- 1
    }
    t_sol_df <- t(sol_df)
    # Base columns in a settings data frame
    sdf_cols <- c(
        "solution",
        "alpha",
        "k",
        "t",
        "snf_scheme",
        "clust_alg",
        "cnt_dist",
        "dsc_dist",
        "ord_dist",
        "cat_dist",
        "mix_dist"
    )
    p <- progressr::progressor(steps = length(features(dl)))
    if (processes == "max") {
        processes <- max(future::availableCores())
    }
    if (processes > 1) {
        future::plan(future::multisession, workers = processes)
        apply_fn <- future.apply::future_lapply
    } else {
        apply_fn <- lapply
    }
    all_nmi_list <- apply_fn(
        colnames(dl_df)[-1],
        function(x) {
            mini_dl <- data_list(
                list(
                    data = dl_df[, c("uid", x)],
                    name = name_lookup[x][[1]],
                    domain = "x",
                    type = type_lookup[x][[1]]
                ),
                uid = "uid"
            )
            this_sc <- sc
            new_sc_cols <- c(
                sdf_cols,
                paste0("inc_", name_lookup[features(mini_dl)][[1]])
            )
            this_sc$"settings_df" <- this_sc$"settings_df"[, new_sc_cols]
            mini_sol_df <- batch_snf(
                mini_dl,
                this_sc
            )
            mini_t_sol_df <- t(mini_sol_df)
            nmi_feature_list <- lapply(
                (seq_len(ncol(mini_t_sol_df) - 1) + 1),
                function(j) {
                    SNFtool::calNMI(
                        mini_t_sol_df[, j],
                        t_sol_df[, j]
                    )
                }
            )
            p()
            return(nmi_feature_list)
        }
    )
    names(all_nmi_list) <- features(dl)
    data_matrix <- do.call(rbind, lapply(all_nmi_list, function(x) unlist(x)))
    # Add the row names as a column
    nmi_df <- data.frame(
        feature = rownames(data_matrix),
        data_matrix,
        row.names = NULL
    )
    colnames(nmi_df) <- c("feature", paste0("s", sc$"settings_df"$"solution"))
    return(nmi_df)
}
