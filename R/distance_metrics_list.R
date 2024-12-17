#' Build a distance metrics list
#'
#' The distance metrics list object (inherits classes `distance_metrics_list`
#' and `list`) is a list that stores R functions which can convert a data
#' frame of features into a matrix of pairwise distances. The list is a nested
#' one, where the first layer of the list can hold up to 5 items (one for each
#' of the metasnf recognized feature types, continuous, discrete, ordinal,
#' categorical, and mixed), and the second layer can hold an arbitrary number
#' of distance functions for each of those types.
#'
#' @param cnt_dist_fns A named list of continuous distance metric functions.
#' @param dsc_dist_fns A named list of discrete distance metric functions.
#' @param ord_dist_fns A named list of ordinal distance metric functions.
#' @param cat_dist_fns A named list of categorical distance metric functions.
#' @param mix_dist_fns A named list of mixed distance metric functions.
#' @param use_defaults If TRUE, prepend the base distance metrics (euclidean
#'  distance for continuous, discrete, and ordinal data and gower distance for
#'  categorical and mixed data) to the resulting distance metrics list.
#' @return A distance metrics list object.
#' @examples
#' # Using just the base distance metrics  ------------------------------------
#' distance_metrics_list <- distance_metrics_list()
#'
#' # Adding your own metrics --------------------------------------------------
#' # This will contain only the and user-provided distance function:
#' my_distance_metric <- function(df) {
#'     # your code that converts a dataframe to a distance metric here...
#'     # return(distance_metric)
#' }
#'
#' distance_metrics_list <- distance_metrics_list(
#'     cnt_dist_fns = list(
#'          "my_distance_metric" = my_distance_metric
#'     )
#' )
#'
#' # Using default base metrics------------------------------------------------
#' # This will contain user-provided and default distance functions:
#' distance_metrics_list <- distance_metrics_list(
#'     cnt_dist_fns = list(
#'          "my_distance_metric" = my_distance_metric
#'     ),
#'     dsc_dist_fns = list(
#'          "my_distance_metric" = my_distance_metric
#'     ),
#'     ord_dist_fns = list(
#'          "my_distance_metric" = my_distance_metric
#'     ),
#'     cat_dist_fns = list(
#'          "my_distance_metric" = my_distance_metric
#'     ),
#'     mix_dist_fns = list(
#'          "my_distance_metric" = my_distance_metric
#'     ),
#'     use_defaults = TRUE
#' )
#' @export
distance_metrics_list <- function(cnt_dist_fns = NULL,
                                  dsc_dist_fns = NULL,
                                  ord_dist_fns = NULL,
                                  cat_dist_fns = NULL,
                                  mix_dist_fns = NULL,
                                  use_defaults = FALSE) {
    # Initialize distance metrics list-like object from user-provided functions
    dmll <- list(
        "cnt_dist_fns" = cnt_dist_fns,
        "dsc_dist_fns" = dsc_dist_fns,
        "ord_dist_fns" = ord_dist_fns,
        "cat_dist_fns" = cat_dist_fns,
        "mix_dist_fns" = mix_dist_fns
    )
    # Remove NULL elements
    dmll <- dmll[lengths(dmll) != 0]
    # Add default metrics if requested `use_defaults` is TRUE
    if (use_defaults) {
        base_cnt_dist_fns <- list("euclidean_distance" = euclidean_distance)
        base_dsc_dist_fns <- list("euclidean_distance" = euclidean_distance)
        base_ord_dist_fns <- list("euclidean_distance" = euclidean_distance)
        base_cat_dist_fns <- list("gower_distance" = gower_distance)
        base_mix_dist_fns <- list("gower_distance" = gower_distance)
        dmll$"cnt_dist_fns" <- c(base_cnt_dist_fns, dmll$"cnt_dist_fns") 
        dmll$"dsc_dist_fns" <- c(base_dsc_dist_fns, dmll$"dsc_dist_fns")
        dmll$"ord_dist_fns" <- c(base_ord_dist_fns, dmll$"ord_dist_fns")
        dmll$"cat_dist_fns" <- c(base_cat_dist_fns, dmll$"cat_dist_fns")
        dmll$"mix_dist_fns" <- c(base_mix_dist_fns, dmll$"mix_dist_fns")
    }
    dmll <- validate_distance_metrics_list(dmll)
    dml <- new_distance_metrics_list(dmll)
    return(dml)
}

validate_distance_metrics_list <- function(dmll) {
    # 1. First layer of items are all functions
    ## Check that all provided functions have names
    #all_metrics_are_named <- dmll |>
    #    lapply(
    #        function(x) {
    #            sum(nchar(names(x)) > 0) == length(x)
    #        }
    #    ) |>
    #    unlist() |>
    #    all()
    #if (!all_metrics_are_named) {
    #    metasnf_error("Please specify a name for every supplied metric.")
    #}
    return(dmll)
}

new_distance_metrics_list <- function(dmll) {
    dml <- structure(dmll, class = c("distance_metrics_list", "list"))
    return(dml)
}

#' Summarize metrics contained in a distance_metrics_list
#'
#' @param distance_metrics_list A distance_metrics_list.
#'
#' @return "data.frame"-class object summarizing items in a distance metrics
#' list.
#'
#' @export
summarize_dml <- function(distance_metrics_list) {
    dml_summary <- lapply(distance_metrics_list, names)
    return(dml_summary)
}

#' Distance metric: Euclidean distance
#'
#' @param df Dataframe containing at least 1 data column
#' @param weights_row Single-row dataframe where the column names contain the
#'  column names in df and the row contains the corresponding weights_row.
#'
#' @return distance_matrix A distance matrix.
#'
#' @export
euclidean_distance <- function(df, weights_row) {
    weights <- diag(weights_row, nrow = length(weights_row))
    weighted_df <- as.matrix(df) %*% weights
    distance_matrix <- weighted_df |>
        stats::dist(method = "euclidean") |>
        as.matrix()
    return(distance_matrix)
}

#' Distance metric: Gower distance
#'
#' @param df Dataframe containing at least 1 data column.
#' @param weights_row For compatibility - function does not accept weights.
#'
#' @return distance_matrix A distance matrix.
#'
#' @export
gower_distance <- function(df, weights_row) {
    df <- char_to_fac(df)
    distance_matrix <- df |>
        cluster::daisy(metric = "gower", warnBin = FALSE) |>
        as.matrix()
    return(distance_matrix)
}

#' Distance metric: Standard normalization then Euclidean
#'
#' @param df Dataframe containing at least 1 data column.
#' @param weights_row Single-row dataframe where the column names contain the
#'  column names in df and the row contains the corresponding weights.
#'
#' @return distance_matrix A distance matrix.
#'
#' @export
sn_euclidean_distance <- function(df, weights_row) {
    df <- SNFtool::standardNormalization(df)
    weights <- diag(weights_row, nrow = length(weights_row))
    weighted_df <- as.matrix(df) %*% weights
    distance_matrix <- weighted_df |>
        stats::dist(method = "euclidean") |>
        as.matrix()
    return(distance_matrix)
}

#' Squared (including weights) Euclidean distance
#'
#' @param df Dataframe containing at least 1 data column.
#' @param weights_row Single-row dataframe where the column names contain the
#'  column names in df and the row contains the corresponding weights.
#'
#' @return distance_matrix A distance matrix.
#'
#' @export
siw_euclidean_distance <- function(df, weights_row) {
    weights <- diag(weights_row, nrow = length(weights_row))
    weighted_df <- as.matrix(df) %*% weights
    distance_matrix <- weighted_df |>
        stats::dist(method = "euclidean") |>
        as.matrix()
    distance_matrix <- distance_matrix^2
    return(distance_matrix)
}

#' Squared (excluding weights) Euclidean distance
#'
#' @param df Dataframe containing at least 1 data column.
#' @param weights_row Single-row dataframe where the column names contain the
#'  column names in df and the row contains the corresponding weights.
#'
#' @return distance_matrix A distance matrix.
#'
#' @export
sew_euclidean_distance <- function(df, weights_row) {
    weights <- diag(weights_row, nrow = length(weights_row))
    weights <- sqrt(weights)
    weighted_df <- as.matrix(df) %*% weights
    distance_matrix <- weighted_df |>
        stats::dist(method = "euclidean") |>
        as.matrix()
    distance_matrix <- distance_matrix^2
    return(distance_matrix)
}

#' Distance metric: Hamming distance
#'
#' @param df Dataframe containing one uid column in the first column and
#'  at least 1 categorical data column. All feature data should be categorical.
#' @param weights_row Single-row dataframe where the column names contain the
#'  column names in df and the row contains the corresponding weights.
#'
#' @return distance_matrix A distance matrix.
#'
#' @export
hamming_distance <- function(df, weights_row) {
    weights <- t(weights_row)
    weights <- weights[, 1]
    distance_matrix <- sapply(
        seq_len(nrow(df)),
        function(i) {
            sapply(
                seq_len(nrow(df)),
                function(j) {
                    apply_condition <- df[i, ] != df[j, ]
                    apply_condition <- as.numeric(apply_condition)
                    return(apply_condition %*% weights)
                }
            )
        }
    )
    return(distance_matrix)
}
