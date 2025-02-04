#' Linearly correct data list by features with unwanted signal
#'
#' Given a data list to correct and another data list of categorical features
#' to linearly adjust for, corrects the first data list based on the residuals
#' of the linear model relating the numeric features in the first data list
#' to the unwanted signal features in the second data list.
#'
#' @param dl A nested list of input data from `data_list()`.
#' @param unwanted_signal_list A data list of categorical features that should
#'  have their mean differences removed in the first data list.
#' @param sig_digs Number of significant digits to round the residuals to.
#' @return A data list ("list") in which each data component has been converted
#'  to contain residuals off of the linear model built against the features in
#'  the unwanted_signal_list.
#' @export
#' @examples
#' has_tutor <- sample(c(1, 0), size = 9, replace = TRUE)
#' math_score <- 70 + 30 * has_tutor + rnorm(9, mean = 0, sd = 5)
#' 
#' math_df <- data.frame(uid = paste0("id_", 1:9), math = math_score)
#' tutor_df <- data.frame(uid = paste0("id_", 1:9), tutor = has_tutor)
#' 
#' dl <- data_list(
#'     list(math_df, "math_score", "school", "continuous"),
#'     uid = "uid"
#' )
#' 
#' adjustment_dl <- data_list(
#'     list(tutor_df, "tutoring", "school", "categorical"),
#'     uid = "uid"
#' )
#' 
#' adjusted_dl <- linear_adjust(dl, adjustment_dl)
#' 
#' adjusted_dl[[1]]$"data"$"math"
#' 
#' # Equivalent to:
#' as.numeric(resid(lm(math_score ~ has_tutor)))
linear_adjust <- function(dl, unwanted_signal_list, sig_digs = NULL) {
    ###########################################################################
    # 1. Check to ensure the patients match
    ###########################################################################
    dl_df <- as.data.frame(dl)
    usl_df <- as.data.frame(unwanted_signal_list)
    if (!identical(dl_df$"uid", usl_df$"uid")) {
        metasnf_error(
            "dl and unwanted_signal_list do not contain same patients."
        )
    }
    ###########################################################################
    # 2. Adjustment
    ###########################################################################
    # Dataframe containing the features to adjust and to adjust by
    full_df <- dplyr::inner_join(dl_df, usl_df, by = "uid")
    unwanted_vars <- colnames(usl_df)[colnames(usl_df) != "uid"]
    # The right hand side of the linear model formula
    rhs <- paste0(unwanted_vars, collapse = " + ")
    # Outer lapply operates on each component of the data list
    numeric_vectors <- c("continuous", "discrete", "numeric", "ordinal")
    adjusted_dl <- dl |> dlapply(
        function(x) {
            if (x$"type" %in% numeric_vectors) {
                non_sub_cols <- colnames(x$"data") != "uid"
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
    return(adjusted_dl)
}
