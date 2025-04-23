#' Summary method for class `ari_matrix`
#'
#' Provides a summary of the `ari_matrix` class object, including the
#' distribution of the adjusted Rand index (ARI) values and the number of
#' solutions.
#'
#' @param object A `ari_matrix` class object.
#' @param ... Other arguments passed to `summary` (not used in this function).
#' @return A named list containing the number of solutions and the distribution
#' of ARI values.
#' @export
summary.ari_matrix <- function(object, ...) {
    ari_quantiles <- stats::quantile(object)
    ari_dim <- dim(object)[[1]]
    return(
        list(
            "solutions" = ari_dim,
            "distribution" = ari_quantiles
        )
    )
}

#' Summary method for class `clust_fns_list`
#'
#' This summary function simply returns to the console the number of functions
#' contained in the `clust_fns_list` object.
#'
#' @param object A `clust_fns_list` class object.
#' @param ... Other arguments passed to `summary` (not used in this function).
#' @return Returns no value. Outputs a message to the console.
#' @export
summary.clust_fns_list <- function(object, ...) {
    return(
        list(
            "length" = length(object)
        )
    )
}

#' Summary method for class `dist_fns_list`
#'
#' This summary function simply returns to the console the number of functions
#' contained in the `dist_fns_list` object.
#'
#' @param object A `dist_fns_list` class object.
#' @param ... Other arguments passed to `summary` (not used in this function).
#' @return Returns no value. Outputs a message to the console.
#' @export
summary.dist_fns_list <- function(object, ...) {
    return(
        list(
            "length" = length(object)
        )
    )
}

#' Summary method for class `ext_solutions_df`
#'
#' This summary function provides a summary of the `ext_solutions_df` class
#' object, including the number of solutions, the distribution of the number of
#' clusters, the number of features, the number of observations, and the
#' distribution of p-values.
#'
#' @param object A `ext_solutions_df` class object.
#' @param ... Other arguments passed to `summary` (not used in this function).
#' @return A named list containing the number of solutions, the distribution of
#' the number of clusters, the number of features, the number of observations,
#' and the distribution of p-values.
#' @export    
summary.ext_solutions_df <- function(object, ...) {
    df <- as.data.frame(object)
    nclust_table <- df$"nclust" |> table()
    keep_cols <- setdiff(
        colnames(df),
        c("min_pval", "mean_pval", "max_pval")
    )
    df <- df[, keep_cols] |>
        dplyr::select(dplyr::ends_with("_pval"))
    pvals <- as.numeric(unlist(df))
    ecdf_vec <- stats::ecdf(pvals)
    p_qt <- ecdf_vec(0.05)
    quantiles <- stats::quantile(
        pvals,
        probs = sort(c(p_qt, 0, 0.25, 0.5, 0.75, 1))
    )
    return(
        list(
            "n_solutions" = nrow(df),
            "nclust_distribution" = nclust_table,
            "n_features" = length(features(object)),
            "n_observations" = length(uids(object)),
            "p_value_distribution" = quantiles
        )
    )
}

#' Summary method for class `settings_df`
#'
#' This summary function provides a summary of the `settings_df` class
#' object, including the number of settings, the distribution of alpha values,
#' the distribution of k values, and the distribution of clustering functions.
#'
#' @param object A `settings_df` class object.
#' @param ... Other arguments passed to `summary` (not used in this function).
#' @return A named list containing summary information of the settings data
#'  frame.
#' @export
summary.settings_df <- function(object, ...) {
    alpha_dist <- stats::quantile(object$"alpha")
    k_dist <- stats::quantile(object$"k")
    scheme_dist <- table(object$"snf_scheme")
    clst_fn_dist <- table(object$"clust_alg")
    cnt_dist_dist <- table(object$"cnt_dist")
    dsc_dist_dist <- table(object$"dsc_dist")
    ord_dist_dist <- table(object$"ord_dist")
    cat_dist_dist <- table(object$"cat_dist")
    mix_dist_dist <- table(object$"mix_dist")
    return(
        list(
            "n_settings" = nrow(object),
            "alpha_distribution" = alpha_dist,
            "k_distribution" = k_dist,
            "scheme_distribution" = scheme_dist,
            "clust_fn_distribution" = clst_fn_dist,
            "cnt_dist_distribution" = cnt_dist_dist,
            "dsc_dist_distribution" = dsc_dist_dist,
            "ord_dist_distribution" = ord_dist_dist,
            "cat_dist_distribution" = cat_dist_dist,
            "mix_dist_distribution" = mix_dist_dist
        )
    )
}

#' Summary method for class `sim_mats_list`
#'
#' This summary function simply returns to the console the number of functions
#' contained in the `sim_mats_list` object.
#'
#' @param object A `sim_mats_list` class object.
#' @param ... Other arguments passed to `summary` (not used in this function).
#' @return Returns no value. Outputs a message to the console.
#' @export
summary.sim_mats_list <- function(object, ...) {
    cat("A list of", length(object), "similarity matrices.\n")
}

#' Summary method for class `snf_config`
#'
#' This summary function provides a summary of the `snf_config` class object,
#' including the settings data frame, clustering functions list, distance
#' functions list, and weights matrix.
#'
#' @param object A `snf_config` class object.
#' @param ... Other arguments passed to `summary` (not used in this function).
#' @return A named list containing the summaries of objects within the config.
#' @export
summary.snf_config <- function(object, ...) {
    sdf <- object$"settings_df"
    cfl <- object$"clust_fns_list"
    dfl <- object$"dist_fns_list"
    wm <- object$"weights_matrix"
    return(
        list(
            "settings_df" = summary(sdf),
            "clust_fns_list" = summary(cfl),
            "dist_fns_list" = summary(dfl),
            "weights_matrix" = summary(wm)
        )
    )
}

#' Summary method for class `solutions_df`
#'
#' This summary function provides a summary of the `solutions_df` class
#' object, including the number of solutions, the distribution of the number of
#' clusters, and the number of observations.
#'
#' @param object A `ext_solutions_df` class object.
#' @param ... Other arguments passed to `summary` (not used in this function).
#' @return A named list containing the number of solutions, the distribution of
#' the number of clusters, and the number of observations.
#' @export
summary.solutions_df <- function(object, ...) {
    df <- as.data.frame(object)
    nclust_table <- df$"nclust" |> table()
    return(
        list(
            "n_solutions" = nrow(df),
            "nclust_distribution" = nclust_table,
            "n_observations" = length(uids(object))
        )
    )
}

#' Summary method for class `data_list`
#'
#' Returns a data list summary (`data.frame` class object) containing
#' information on components, features, variable types, domains, and component
#' dimensions.
#'
#' @param object A `data_list` class object.
#' @param scope The level of detail for the summary. By default, this is set to
#'  "component", which returns a summary of the data list at the component 
#'  level. Can also be set to "feature", resulting in a summary at the feature
#'  level.
#' @param ... Other arguments passed to `summary` (not used in this function)
#' @return A `data.frame` class object. If `scope` is "component", each row
#'  shows the name, variable type, domain, and dimensions of each component. If
#'  `scope` is "feature", each row shows the name, variable type, and domain of
#'  each feature.
#' @export
summary.data_list <- function(object, scope = "component", ...) {
    if (scope == "component") {
        dl_summary <- data.frame(
            name = unlist(lapply(object, function(x) x$"name")),
            type = unlist(lapply(object, function(x) x$"type")),
            domain = unlist(domains(object)),
            length = unlist(lapply(object, function(x) dim(x$"data")[1])),
            width = unlist(lapply(object, function(x) dim(x$"data")[2] - 1))
        )
    } else if (scope == "feature") {
        dl_df <- as.data.frame(object)
        dl_df <- dl_df[, colnames(dl_df) != "uid", drop = FALSE]
        types <- object |>
            lapply(
                function(x) {
                    rep(x$"type", ncol(x$"data") - 1)
                }
            ) |>
            unlist()
        domains <- object |>
            lapply(
                function(x) {
                    rep(x$"domain", ncol(x$"data") - 1)
                }
            ) |>
            unlist()
        dl_summary <- data.frame(
            name = colnames(dl_df),
            type = types,
            domain = domains
        )
    }
    rownames(dl_summary) <- seq_len(nrow(dl_summary))
    return(dl_summary)
}

#' Summary method for class `t_solutions_df`
#'
#' This summary function provides a summary of the `t_solutions_df` class
#' object, including the number of solutions, the distribution of the number of
#' clusters, the number of features, the number of observations, and the
#' distribution of p-values.
#'
#' @param object A `t_solutions_df` class object.
#' @param ... Other arguments passed to `summary` (not used in this function).
#' @return A named list containing the number of solutions, the distribution of
#' the number of clusters, the number of features, the number of observations,
#' and the distribution of p-values.
#' @export    
summary.t_solutions_df <- function(object, ...) {
    sdf <- t(object)
    return(summary(sdf))
}

#' Summary method for class `t_ext_solutions_df`
#'
#' This summary function provides a summary of the `t_ext_solutions_df` class
#' object, including the number of solutions, the distribution of the number of
#' clusters, the number of features, the number of observations, and the
#' distribution of p-values.
#'
#' @param object A `t_ext_solutions_df` class object.
#' @param ... Other arguments passed to `summary` (not used in this function).
#' @return A named list containing the number of solutions, the distribution of
#' the number of clusters, the number of features, the number of observations,
#' and the distribution of p-values.
#' @export    
summary.t_ext_solutions_df <- function(object, ...) {
    esdf <- t(object)
    return(summary(esdf))
}

#' Summary method for class `weights_matrix`
#'
#' This summary function provides a summary of the `weights_matrix` class
#' object, including the minimum, maximum, mean, and standard deviation of the
#' feature weights.
#'
#' @param object A `weights_matrix` class object.
#' @param ... Other arguments passed to `summary` (not used in this function).
#' @return A named list containing the summary statistics of the weights matrix,
#'  the number of solutions, and the number of features.
#' @export
summary.weights_matrix <- function(object, ...) {
    summary_stats <- data.frame(
        feature = colnames(object),
        min = apply(object, 2, min),
        max = apply(object, 2, max),
        mean = colMeans(object),
        sd = apply(object, 2, stats::sd)
    )
    rownames(summary_stats) <- NULL
    return(
        list(
            "summary_stats" = summary_stats,
            "n_solutions" = nrow(object),
            "n_features" = ncol(object)
        )
    )
    return(summary_stats)
}
