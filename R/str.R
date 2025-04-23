str.ari_matrix <- function(object, ...) {
    cat("ari_matrix [Pairwise ARIs for ", nrow(object), " solutions]: ", sep = "")
    cat(head(as.numeric(object)))
    cat("...\n")
}

str.clust_fns_list <- function(object, ...) {
}

str.data_list <- function(object, ...) {
    n_types <- length(unique(object$"type"))
    if (n_types > 1) {
        suffix <- "s"
    } else {
        suffix <- ""
    }
    cat(
        "data_list [",
        n_observations(object), " observations, ",
        length(object), " data frames, ",
        length(attributes(object)$"domains"), " domains, ",
        length(unique(summary(object)$"type")), " type", suffix, "]\n",
        sep = ""
    )
}
str(mock_data_list)

str.dist_fns_list <- function(object, ...) {
}

str.ext_solutions_df <- function(object, ...) {
}

str.settings_df <- function(object, ...) {
}

str.sim_mats_list <- function(object, ...) {
}

str.snf_config <- function(object, ...) {
}

str.solutions_df <- function(object, ...) {
}

str.t_ext_solutions_df <- function(object, ...) {
}

str.t_solutions_df <- function(object, ...) {
}

#' Display structure of a weights matrix
#'
#' @param object A `weights_matrix` class object.
#' @param ... Additional arguments (not used).
#' @return Does not return an object; outputs object structure to console.
#' @export
str.weights_matrix <- function(object, ...) {
    cat("weights_matrix [", nrow(object), " solutions x ", ncol(object), " features]: ", sep = "")
    cat(head(as.numeric(object)))
    cat("...\n")
}

str(mock_ari_matrix)

str(mock_weights_matrix)


mock_data_list

