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
    weights <- format_weights_row(weights_row)
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
    weights <- format_weights_row(weights_row)
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
    weights <- format_weights_row(weights_row)
    weighted_df <- as.matrix(df) %*% weights
    # fix this
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
    weights <- format_weights_row(weights_row)
    weights <- sqrt(weights)
    weighted_df <- as.matrix(df) %*% weights
    # fix this
    distance_matrix <- weighted_df |>
        stats::dist(method = "euclidean") |>
        as.matrix()
    distance_matrix <- distance_matrix^2
    return(distance_matrix)
}

#' Distance metric: Hamming distance
#'
#' @param df Dataframe containing one subjectkey column in the first column and
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
    distance_matrix = sapply(
        1:nrow(df),
        function(i) {
            sapply(
                1:nrow(df),
                function(j) {
                    apply_condition <- df[i, ] != df[j, ]
                    apply_condition <- as.numeric(apply_condition)
                    print(apply_condition)
                    print(weights)
                    return(apply_condition %*% weights)
                }
            )
        }
    )
    return(distance_matrix)
}
