#' Generate a matrix to store feature weights
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @param nrow Number of rows to generate the template weights matrix for.
#'
#' @param fill String indicating what to populate generate rows with. Can be
#'  "ones" (default; fill matrix with 1), "uniform" (fill matrix with uniformly
#'  distributed random values), or "exponential" (fill matrix with
#'  exponentially distributed random values).
#'
#' @return weights_matrix A properly formatted matrix containing columns for
#'  all the features that require weights and rows.
#'
#' @export
generate_weights_matrix <- function(data_list = NULL,
                                    nrow = 1,
                                    fill = "ones") {
    matrix_colnames <- data_list |>
        lapply(
            function(x) {
                colnames(x$"data")[colnames(x$"data") != "subjectkey"]
            }
        ) |>
        unlist()
    if (fill == "ones") {
        fill <- 1
    } else if (fill == "uniform") {
        fill <- stats::runif(nrow * length(matrix_colnames))
    } else if (fill == "exponential") {
        fill <- stats::rexp(nrow * length(matrix_colnames))
    }
    matrix_base <- matrix(
        nrow = nrow,
        ncol = length(matrix_colnames),
        data = fill
    )
    colnames(matrix_base) <- matrix_colnames
    matrix_base
}

#' Properly format a vector of weights for matrix multiplication
#'
#' Wrapper around the diag() function. This function was created because diag()
#' applied to a scalar doesn't spit that scalar back out, but instead returns
#' identity matrix of dimension (scalar * scalar).
#'
#' @param weights_row Vector (possibly size 1) of weights.
#'
#' @export
format_weights_row <- function(weights_row) {
    if (length(weights_row) == 1) {
        return(weights_row)
    } else {
        return(diag(weights_row))
    }
}
