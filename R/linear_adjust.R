#' Linearly correct data_list by features with unwanted signal
#'
#' Given a data_list to correct and another data_list of categorical features
#' to linearly adjust for, corrects the first data_list based on the residuals
#' of the linear model relating the numeric features in the first data_list
#' to the unwanted signal features in the second data list.
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @param unwanted_signal_list A data_list of categorical features that should
#' have their mean differences removed in the first data_list.
#'
#' @param sig_digs Number of significant digits to round the residuals to.
#'
#' @return A data list ("list") in which each data component has been converted
#' to contain residuals off of the linear model built against the features in
#' the unwanted_signal_list.
#'
#' @export
linear_adjust <- function(data_list, unwanted_signal_list, sig_digs = NULL) {
    ###########################################################################
    # 1. Check to ensure the patients match
    ###########################################################################
    dl_df <- collapse_dl(data_list)
    usl_df <- collapse_dl(unwanted_signal_list)
    if (!identical(dl_df$"subjectkey", usl_df$"subjectkey")) {
        stop("data_list and unwanted_signal_list do not contain same patients")
    }
    ###########################################################################
    # 2. Adjustment
    ###########################################################################
    # Dataframe containing the features to adjust and to adjust by
    full_df <- dplyr::inner_join(dl_df, usl_df, by = "subjectkey")
    unwanted_vars <- colnames(usl_df)[colnames(usl_df) != "subjectkey"]
    # The right hand side of the linear model formula
    rhs <- paste0(unwanted_vars, collapse = " + ")
    # Outer lapply operates on each component of the data_list
    numeric_vectors <- c("continuous", "discrete", "numeric", "ordinal")
    adjusted_data_list <- data_list |> lapply(
        function(x) {
            if (x$"type" %in% numeric_vectors) {
                non_sub_cols <- colnames(x$"data") != "subjectkey"
                columns <- colnames(x$"data")[non_sub_cols]
                # Inner loop adjusts the numeric columns with their residuals
                # one at a time.
                for (column in columns) {
                    formula <- stats::as.formula(paste0(column, " ~ ", rhs))
                    linear_model <- stats::lm(formula, full_df)
                    adjusted_column <- stats::resid(linear_model)
                    if (!is.null(sig_digs)) {
                        adjusted_column <- signif(adjusted_column, sig_digs)
                    }
                    x$"data"[, column] <- adjusted_column
                }
            }
            return(x)
        }
    )
    return(adjusted_data_list)
}
