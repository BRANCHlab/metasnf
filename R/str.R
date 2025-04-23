#' Structure of a `ari_matrix` object
#'
#' @param object A `ari_matrix` class object.
#' @param ... Additional arguments (not used).
#' @return Does not return an object; outputs object structure to console.
#' @export
str.ari_matrix <- function(object, ...) {
    cat(
        "ari_matrix [Pairwise ARIs for ", nrow(object),
        " solutions]: ", sep = ""
    )
    cat(utils::head(as.numeric(object)))
    cat("...\n")
}

#' Structure of a `clust_fns_list` object
#'
#' @param object A `clust_fns_list` class object.
#' @param ... Additional arguments (not used).
#' @return Does not return an object; outputs object structure to console.
#' @export
str.clust_fns_list <- function(object, ...) {
    cat(
        "clust_fns_list [",
        length(object), " clustering function", pl(length(object)), "]\n",
        sep = ""
    )
}

#' Structure of a `data_list` object
#'
#' @param object A `data_list` class object.
#' @param ... Additional arguments (not used).
#' @return Does not return an object; outputs object structure to console.
#' @export
str.data_list <- function(object, ...) {
    n_observations <- attributes(object)$"n_observations"
    n_types <- length(unique(summary(object)$"type"))
    n_domains <- length(attributes(object)$"domains")
    n_dfs <- length(object)
    cat(
        "data_list [",
        n_observations, " observation", pl(n_observations), ", ",
        n_dfs, " data frame", pl(n_dfs), ", ",
        n_domains, " domain", pl(n_domains), ", ",
        n_types, " type", pl(n_types), "]\n",
        sep = ""
    )
}

#' Structure of a `dist_fns_list` object
#'
#' @param object A `dist_fns_list` class object.
#' @param ... Additional arguments (not used).
#' @return Does not return an object; outputs object structure to console.
#' @export
str.dist_fns_list <- function(object, ...) {
    cat(
        "dist_fns_list [",
        length(object), " distance function", pl(length(object)), "]\n",
        sep = ""
    )
}

#' Structure of a `ext_solutions_df` object
#'
#' @param object A `ext_solutions_df` class object.
#' @param ... Additional arguments (not used).
#' @return Does not return an object; outputs object structure to console.
#' @export
str.ext_solutions_df <- function(object, ...) {
    n_solutions <- nrow(object)
    n_observations <- length(uids(object))
    n_sfts <- length(attr(object, "summary_features"))
    n_fts <- length(attr(object, "features"))
    n_clusts <- object$"nclust"
    cat(
        "ext_solutions_df [",
        n_solutions, " solution", pl(n_solutions), ", ",
        n_observations, " observation", pl(n_observations), ", ",
        "clusters: ", min(n_clusts), "-", max(n_clusts), ", ",
        n_fts, " feature", pl(n_fts), " (",
        n_sfts, " summary feature", pl(n_sfts),
        ")]\n",
        sep = ""
    )
}

#' Structure of a `settings_df` object
#'
#' @param object A `settings_df` class object.
#' @param ... Additional arguments (not used).
#' @return Does not return an object; outputs object structure to console.
#' @export
str.settings_df <- function(object, ...) {
    n_solutions <- nrow(object)
    n_schemes <- length(unique(object$"snf_scheme"))
    inc_df <- dplyr::select(object, dplyr::starts_with("inc_"))
    pct_inc <- mean(as.numeric(unlist(inc_df)))
    cat(
        "settings_df [",
        n_solutions, " solution", pl(n_solutions), ", ",
        "alpha: ", min(object$"alpha"), "-", max(object$"alpha"), ", ",
        "k: ", min(object$"k"), "-", max(object$"k"), ", ",
        "t: ", min(object$"t"), "-", max(object$"t"), ", ",
        n_schemes, " scheme", pl(n_schemes), ", ",
        "dropout: ", round(100*(1 - pct_inc), 1),
        "%]\n",
        sep = ""
    )
}

#' Structure of a `sim_mats_list` object
#'
#' @param object A `sim_mats_list` class object.
#' @param ... Additional arguments (not used).
#' @return Does not return an object; outputs object structure to console.
#' @export
str.sim_mats_list <- function(object, ...) {
    n_mats <- length(object)
    if (n_mats >= 1) {
        dim_statement <- paste0(
            "size: ", dim(object[[1]])[[1]]
        )
    } else {
        dim_statement <- ""
    }
    plural <- if (n_mats == 1) "matrix" else "matrices"
    cat(
        "sim_mats_list [",
        n_mats, " ", plural, ", ",
        dim_statement,
        "]\n",
        sep = ""
    )
}

#' Structure of a `snf_config` object
#'
#' @param object A `snf_config` class object.
#' @param ... Additional arguments (not used).
#' @return Does not return an object; outputs object structure to console.
#' @export
str.snf_config <- function(object, ...) {
    cat("snf_config:\n")
    cat("    ")
    str.settings_df(object$"settings_df")
    cat("  ")
    str.dist_fns_list(object$"dist_fns_list")
    cat(" ")
    str.clust_fns_list(object$"clust_fns_list")
    cat(" ")
    str.weights_matrix(object$"weights_matrix")
}

#' Structure of a `solutions_df` object
#'
#' @param object A `solutions_df` class object.
#' @param ... Additional arguments (not used).
#' @return Does not return an object; outputs object structure to console.
#' @export
str.solutions_df <- function(object, ...) {
    n_solutions <- nrow(object)
    n_observations <- length(uids(object))
    n_sfts <- length(attr(object, "summary_features"))
    n_fts <- length(attr(object, "features"))
    n_clusts <- object$"nclust"
    cat(
        "solutions_df [",
        n_solutions, " solution", pl(n_solutions), ", ",
        n_observations, " observation", pl(n_observations),", ",
        "clusters: ", min(n_clusts), "-", max(n_clusts),
        "]\n",
        sep = ""
    )
}

#' Structure of a `t_ext_solutions_df` object
#'
#' @param object A `t_ext_solutions_df` class object.
#' @param ... Additional arguments (not used).
#' @return Does not return an object; outputs object structure to console.
#' @export
str.t_ext_solutions_df <- function(object, ...) {
    t_object <- t(object)
    cat("t_")
    str.ext_solutions_df(t_object)
}

#' Structure of a `t_solutions_df` object
#'
#' @param object A `t_solutions_df` class object.
#' @param ... Additional arguments (not used).
#' @return Does not return an object; outputs object structure to console.
#' @export
str.t_solutions_df <- function(object, ...) {
    t_object <- t(object)
    cat("t_")
    str.solutions_df(t_object)
}

#' Structure of a `weights_matrix` object
#'
#' @param object A `weights_matrix` class object.
#' @param ... Additional arguments (not used).
#' @return Does not return an object; outputs object structure to console.
#' @export
str.weights_matrix <- function(object, ...) {
    cat(
        "weights_matrix [",
        nrow(object), " solution", pl(nrow(object)), ", ",
        ncol(object),
        " features]\n",
        sep = ""
    )
}
