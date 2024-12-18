#' Build a distance metrics list
#'
#' The distance metrics list object (inherits classes `dist_fns_list`
#' and `list`) is a list that stores R functions which can convert a data
#' frame of features into a matrix of pairwise distances. The list is a nested
#' one, where the first layer of the list can hold up to 5 items (one for each
#' of the metasnf recognized feature types, continuous, discrete, ordinal,
#' categorical, and mixed), and the second layer can hold an arbitrary number
#' of distance functions for each of those types.
#'
#' Call ?distance_metrics to see all distance metric functions provided in
#' metasnf.
#'
#' @param cnt_dist_fns A named list of continuous distance metric functions.
#' @param dsc_dist_fns A named list of discrete distance metric functions.
#' @param ord_dist_fns A named list of ordinal distance metric functions.
#' @param cat_dist_fns A named list of categorical distance metric functions.
#' @param mix_dist_fns A named list of mixed distance metric functions.
#' @param use_default_dist_fns If TRUE, prepend the base distance metrics
#'  (euclidean distance for continuous, discrete, and ordinal data and gower
#'  distance for categorical and mixed data) to the resulting distance metrics
#'  list.
#' @return A distance metrics list object.
#' @examples
#' # Using just the base distance metrics  ------------------------------------
#' dist_fns_list <- dist_fns_list()
#'
#' # Adding your own metrics --------------------------------------------------
#' # This will contain only the and user-provided distance function:
#' cubed_euclidean <- function(df, weights_row) {
#'     # (your code that converts a dataframe to a distance metric here...)
#'     weights <- diag(weights_row, nrow = length(weights_row))
#'     weighted_df <- as.matrix(df) %*% weights
#'     distance_matrix <- weighted_df |>
#'         stats::dist(method = "euclidean") |>
#'         as.matrix()
#'     distance_matrix <- distance_matrix^3
#'     return(distance_matrix)
#' }
#'
#' dist_fns_list <- dist_fns_list(
#'     cnt_dist_fns = list(
#'          "my_cubed_euclidean" = cubed_euclidean
#'     )
#' )
#'
#' # Using default base metrics------------------------------------------------
#' # Call ?distance_metrics to see all distance metric functions provided in
#' # metasnf. The code below will contain a mix of user-provided and built-in
#' # distance metric functions.
#' dist_fns_list <- dist_fns_list(
#'     cnt_dist_fns = list(
#'          "my_distance_metric" = cubed_euclidean
#'     ),
#'     dsc_dist_fns = list(
#'          "my_distance_metric" = cubed_euclidean
#'     ),
#'     ord_dist_fns = list(
#'          "my_distance_metric" = cubed_euclidean
#'     ),
#'     cat_dist_fns = list(
#'          "my_distance_metric" = gower_distance
#'     ),
#'     mix_dist_fns = list(
#'          "my_distance_metric" = gower_distance
#'     ),
#'     use_default_dist_fns = TRUE
#' )
#' @export
dist_fns_list <- function(cnt_dist_fns = NULL,
                                  dsc_dist_fns = NULL,
                                  ord_dist_fns = NULL,
                                  cat_dist_fns = NULL,
                                  mix_dist_fns = NULL,
                                  use_default_dist_fns = FALSE) {
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
    # Add default metrics if requested `use_default_dist_fns` is TRUE
    if (use_default_dist_fns) {
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
    dmll <- validate_dist_fns_list(dmll)
    dml <- new_dist_fns_list(dmll)
    return(dml)
}

#' Validator for dist_fns_list class object
#'
#' @keywords internal
#' @param dmll A distance metrics list-like list object to be validated.
#' @return If dmll has a valid structure for a `dist_fns_list` class
#'  object, returns the input unchanged. Otherwise, raises an error.
validate_dist_fns_list <- function(dmll) {
    # First layer of items have valid names
    check_dmll_item_names(dmll)
    # Second layer of items are all functions
    check_dmll_subitems_are_fns(dmll)
    # Names in first and second layer are all unique
    check_dmll_unique_names(dmll)
    # Ensure valid function argument names
    check_dmll_fn_args(dmll)
    # Check that all provided functions have names
    check_dmll_fn_names(dmll)
    return(dmll)
}

new_dist_fns_list <- function(dmll) {
    dml <- structure(dmll, class = c("dist_fns_list", "list"))
    return(dml)
}

#' Check if items of a distance metrics list-like object have valid names
#'
#' @keywords internal
#' @inheritParams validate_dist_fns_list
#' @return Doesn't return any value. Raises error if the items of dmll don't
#'  have valid formatted names.
check_dmll_item_names <- function(dmll) {
    valid_names <- c(
        "cnt_dist_fns",
        "dsc_dist_fns",
        "ord_dist_fns",
        "cat_dist_fns",
        "mix_dist_fns"
    )
    items_have_valid_names <- all(names(dmll) %in% valid_names)
    if (!items_have_valid_names) {
        metasnf_error(
            "Distance metrics list item names may only be: 'cnt_dist_fns', 'd",
            "sc_dist_fns', 'ord_dist_fns', 'cat_dist_fns', or 'mix_dist_fns'."
        )
    }
}

#' Check if subitems of a distance metrics list-like object are functions
#'
#' @keywords internal
#' @inheritParams validate_dist_fns_list
#' @return Doesn't return any value. Raises error if the subitems of dmll are
#'  not functions.
check_dmll_subitems_are_fns <- function(dmll) {
    subitems_are_fns <- lapply(
        dmll,
        function(x) {
            lapply(
                x,
                function(y) {
                    inherits(y, "function")
                }
            ) |>
                unlist() |>
                all()
        }
    ) |>
        unlist() |>
        all()
    if (!subitems_are_fns) {
        metasnf_error("Distance metrics list can only store functions.")
    }
}

#' Check if names in a distance metrics list-like object are unique
#'
#' @keywords internal
#' @inheritParams validate_dist_fns_list
#' @return Doesn't return any value. Raises error if the items of dmll aren't
#'  unique across layer 1 or within each item of layer 2.
check_dmll_unique_names <- function(dmll) {
    layer_one_n_names <- length(names(dmll))
    layer_one_n_unique_names <- length(unique(names(dmll)))
    if (layer_one_n_names != layer_one_n_unique_names) {
        metasnf_error(
            "Distance metrics list cannot have duplicate layer 1 names."
        )
    }
    has_unique_layer_two_names <- lapply(
        dmll,
        function(x) {
            length(names(x)) == length(unique(names(x)))
        }
    ) |>
        unlist() |>
        all()
    if (!has_unique_layer_two_names) {
        metasnf_error(
            "Distance metrics list cannot have duplicate layer 2 names."
        )
    }
}

#' Check if functions in a distance metrics list-like have valid arguments
#'
#' @keywords internal
#' @inheritParams validate_dist_fns_list
#' @return Doesn't return any value. Raises error if the functions in dmll
#'  don't have valid arguments.
check_dmll_fn_args <- function(dmll) {
    valid_args <- lapply(
        dmll,
        function(x) {
            lapply(
                x,
                function(y) {
                    methods::formalArgs(y) == c("df", "weights_row")
                }
            ) |>
                unlist() |>
                all()
        }
    ) |>
        unlist() |>
        all()
    if (!valid_args) {
        metasnf_error(
            "Distance metrics list functions must have arguments `df` and `we",
            "ights`."
        )
    }
}

#' Check if functions in a distance metrics list-like have names
#'
#' @keywords internal
#' @inheritParams validate_dist_fns_list
#' @return Doesn't return any value. Raises error if the functions in dmll
#'  don't have names.
check_dmll_fn_names <- function(dmll) {
    fns_have_names <- dmll |>
        lapply(
            function(x) {
                sum(nchar(names(x)) > 0) == length(x)
            }
        ) |>
        unlist() |>
        all()
    if (!fns_have_names) {
        metasnf_error("Please specify a name for every supplied function.")
    }
}

#' Summarize metrics contained in a dist_fns_list
#'
#' @param dist_fns_list A dist_fns_list.
#'
#' @return "data.frame"-class object summarizing items in a distance metrics
#' list.
#'
#' @export
summarize_dml <- function(dist_fns_list) {
    dml_summary <- lapply(dist_fns_list, names)
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
