#' Build a distance metrics list
#'
#' The distance metrics list object (inherits classes `dist_fns_list`
#' and `list`) is a list that stores R functions which can convert a data
#' frame of features into a matrix of pairwise distances. The list is a nested
#' one, where the first layer of the list can hold up to 5 items (one for each
#' of the `metasnf` recognized feature types, continuous, discrete, ordinal,
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
#' @param automatic_standard_normalize If TRUE, will automatically use
#'  standard normalization prior to calculation of any numeric distances. This
#'  parameter overrides all other distance functions list-related parameters.
#' @return A distance metrics list object.
#' @export
#' @examples
#' # Using just the base distance metrics  ------------------------------------
#' dist_fns_list <- dist_fns_list()
#'
#' # Adding your own metrics --------------------------------------------------
#' # This will contain only the and user-provided distance function:
#' cubed_euclidean <- function(df, weights_row) {
#'     # (your code that converts a data frame to a distance metric here...)
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
dist_fns_list <- function(cnt_dist_fns = NULL,
                          dsc_dist_fns = NULL,
                          ord_dist_fns = NULL,
                          cat_dist_fns = NULL,
                          mix_dist_fns = NULL,
                          automatic_standard_normalize = FALSE,
                          use_default_dist_fns = FALSE) {
    if (automatic_standard_normalize) {
        metasnf_alert(
            "Automatic standard normalization requested.",
            " All other distance functions list parameters will be ignored."
        )
        dfll <- list(
            "cnt_dist_fns" = cnt_dist_fns,
            "dsc_dist_fns" = dsc_dist_fns,
            "ord_dist_fns" = ord_dist_fns,
            "cat_dist_fns" = cat_dist_fns,
            "mix_dist_fns" = mix_dist_fns
        )
    } else {
        dfll <- list(
            "cnt_dist_fns" = cnt_dist_fns,
            "dsc_dist_fns" = dsc_dist_fns,
            "ord_dist_fns" = ord_dist_fns,
            "cat_dist_fns" = cat_dist_fns,
            "mix_dist_fns" = mix_dist_fns
        )
        # Remove NULL elements
        dfll <- dfll[lengths(dfll) != 0]
        if (length(dfll) == 0 & !use_default_dist_fns) {
            metasnf_alert(
                "No distance functions specified. Using defaults."
            )
            use_default_dist_fns <- TRUE
        }
        # Add default metrics if requested `use_default_dist_fns` is TRUE
        if (use_default_dist_fns) {
            base_cnt_fns <- list("euclidean_distance" = euclidean_distance)
            base_dsc_fns <- list("euclidean_distance" = euclidean_distance)
            base_ord_fns <- list("euclidean_distance" = euclidean_distance)
            base_cat_fns <- list("gower_distance" = gower_distance)
            base_mix_fns <- list("gower_distance" = gower_distance)
            dfll$"cnt_dist_fns" <- c(base_cnt_fns, dfll$"cnt_dist_fns") 
            dfll$"dsc_dist_fns" <- c(base_dsc_fns, dfll$"dsc_dist_fns")
            dfll$"ord_dist_fns" <- c(base_ord_fns, dfll$"ord_dist_fns")
            dfll$"cat_dist_fns" <- c(base_cat_fns, dfll$"cat_dist_fns")
            dfll$"mix_dist_fns" <- c(base_mix_fns, dfll$"mix_dist_fns")
        }
    }
    dfll <- validate_dist_fns_list(dfll)
    dfl <- new_dist_fns_list(dfll)
    return(dfl)
}

#' Validator for dist_fns_list class object
#'
#' @keywords internal
#' @param dfll A distance metrics list-like list object to be validated.
#' @return If dfll has a valid structure for a `dist_fns_list` class
#'  object, returns the input unchanged. Otherwise, raises an error.
validate_dist_fns_list <- function(dfll) {
    class(dfll) <- setdiff(class(dfll), "dist_fns_list")
    # First layer of items have valid names
    check_dfll_item_names(dfll)
    # Second layer of items are all functions
    check_dfll_subitems_are_fns(dfll)
    # Names in first and second layer are all unique
    check_dfll_unique_names(dfll)
    # Ensure valid function argument names
    check_dfll_fn_args(dfll)
    # Check that all provided functions have names
    check_dfll_fn_names(dfll)
    return(dfll)
}

#' Constructor for `dist_fns_list` class object
#' 
#' @keywords internal
#' @inheritParams validate_dist_fns_list
#' @return A `dist_fns_list` object.
new_dist_fns_list <- function(dfll) {
    dfl <- structure(dfll, class = c("dist_fns_list", "list"))
    return(dfl)
}

#' Check if items of a distance metrics list-like object have valid names
#'
#' @keywords internal
#' @inheritParams validate_dist_fns_list
#' @return Doesn't return any value. Raises error if the items of dfll don't
#'  have valid formatted names.
check_dfll_item_names <- function(dfll) {
    valid_names <- c(
        "cnt_dist_fns",
        "dsc_dist_fns",
        "ord_dist_fns",
        "cat_dist_fns",
        "mix_dist_fns"
    )
    items_have_valid_names <- all(names(dfll) %in% valid_names)
    if (!items_have_valid_names) {
        metasnf_error(
            "Distance metrics list item names may only be: 'cnt_dist_fns', 'd",
            "sc_dist_fns', 'ord_dist_fns', 'cat_dist_fns', or 'mix_dist_fns'."
        )
    }
}

#' Check if items of a distance metrics list-like object are functions
#'
#' @keywords internal
#' @inheritParams validate_dist_fns_list
#' @return Doesn't return any value. Raises error if the items of dfll are
#'  not functions.
check_dfll_subitems_are_fns <- function(dfll) {
    subitems_are_fns <- lapply(
        dfll,
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
#' @return Doesn't return any value. Raises error if the items of dfll aren't
#'  unique across layer 1 or within each item of layer 2.
check_dfll_unique_names <- function(dfll) {
    layer_one_n_names <- length(names(dfll))
    layer_one_n_unique_names <- length(unique(names(dfll)))
    if (layer_one_n_names != layer_one_n_unique_names) {
        metasnf_error(
            "Distance metrics list cannot have duplicate layer 1 names."
        )
    }
    has_unique_layer_two_names <- lapply(
        dfll,
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
#' @return Doesn't return any value. Raises error if the functions in dfll
#'  don't have valid arguments.
check_dfll_fn_args <- function(dfll) {
    valid_args <- lapply(
        dfll,
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
            "Functions in distance functions list must have arguments `df` an",
            "d `weights`."
        )
    }
}

#' Check if functions in a distance metrics list-like have names
#'
#' @keywords internal
#' @inheritParams validate_dist_fns_list
#' @return Doesn't return any value. Raises error if the functions in dfll
#'  don't have names.
check_dfll_fn_names <- function(dfll) {
    fns_have_names <- dfll |>
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

#' Built-in distance functions
#'
#' These functions can be used when building a `metasnf` distance functions
#' list. Each function converts a data frame into to a distance matrix.
#'
#' Functions that work for numeric data: 
#' - euclidean_distance: typical Euclidean distance
#' - sn_euclidean_distance: Data frame is first standardized and normalized
#'   before typical Euclidean distance is applied
#' - siw_euclidean_distance: Squared (including weights) Euclidean distance,
#'   where the weights are also squared
#' - sew_euclidean_distance: Squared (excluding weights) Euclidean distance,
#'   where the weights are not also squared
#'
#' Functions that work for binary data: 
#' - hamming_distance: typical Hamming distance
#'
#' Functions that work for any type of data: 
#' - gower_distance: Gower distance (cluster::daisy)
#'
#' @param df Data frame containing at least 1 data column
#' @param weights_row Single-row data frame where the column names contain the
#'  column names in df and the row contains the corresponding weights_row.
#' @return A matrix class object containing pairwise distances.
#' @name dist_fns
NULL

#' @rdname dist_fns
#' @export
euclidean_distance <- function(df, weights_row) {
    weights <- diag(weights_row, nrow = length(weights_row))
    weighted_df <- as.matrix(df) %*% weights
    distance_matrix <- weighted_df |>
        stats::dist(method = "euclidean") |>
        as.matrix()
    return(distance_matrix)
}

#' @rdname dist_fns
#' @export
gower_distance <- function(df, weights_row) {
    df <- char_to_fac(df)
    distance_matrix <- df |>
        cluster::daisy(metric = "gower", warnBin = FALSE) |>
        as.matrix()
    return(distance_matrix)
}

#' @rdname dist_fns
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
#' @param df data frame containing at least 1 data column.
#' @param weights_row Single-row data frame where the column names contain the
#'  column names in df and the row contains the corresponding weights.
#' @return distance_matrix A distance matrix.
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

#' @rdname dist_fns
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

#' @rdname dist_fns
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
