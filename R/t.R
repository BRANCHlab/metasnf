#' @export
t.solutions_df <- function(x) {
    sol_df <- x
    x <- NextMethod()
    x <- data.frame(x)
    x$"uid" <- rownames(x)
    rownames(x) <- NULL
    x <- x[, unique(c("uid", colnames(x)))]
    colnames(x) <- c("uid", paste0("s", sol_df$"solution"))
    # Remove transposed solution, nclust, and mc labels
    x <- x[-c(1:3), ]
    attributes(x)$"sim_mats_list" <- attributes(sol_df)$"sim_mats_list"
    attributes(x)$"snf_config" <- attributes(sol_df)$"snf_config"
    attributes(x)$"mc_labels" <- sol_df$"mc"
    x <- numcol_to_numeric(x)
    x <- dplyr::mutate(x, dplyr::across(dplyr::starts_with("s"), as.numeric))
    class(x) <- c("t_solutions_df", "data.frame")
    x
}

#' @export
t.t_solutions_df <- function(x) {
    sol_df <- x
    x <- NextMethod()
    x <- data.frame(x)
    colnames(x) <- x[1, ]
    x <- x[-1, ]
    x <- numcol_to_numeric(x) |>
        dplyr::mutate(
            dplyr::across(
                dplyr::starts_with("uid_"), ~ as.integer(.)
            )
        )
    x$"solution" <- as.integer(as.numeric(sub("s", "", rownames(x))))
    x$"nclust" <- x |>
        drop_cols("solution") |>
        apply(1, function(y) length(unique(y))) |>
        as.integer()
    x$"mc" <- as.character(attributes(sol_df)$"mc_labels")
    x <- sol_df_col_order(x)
    class(x) <- c("solutions_df", "data.frame")
    attributes(x)$"sim_mats_list" <- attributes(sol_df)$"sim_mats_list"
    attributes(x)$"snf_config" <- attributes(sol_df)$"snf_config"
    rownames(x) <- NULL
    x
}

#' @export
t.ext_solutions_df <- function(x) {
    ext_sol_df <- x
    x <- gexclude(x, "_pval$")
    x <- NextMethod()
    x <- data.frame(x)
    x$"uid" <- rownames(x)
    rownames(x) <- NULL
    x <- x[, unique(c("uid", colnames(x)))]
    colnames(x) <- c("uid", paste0("s", ext_sol_df$"solution"))
    x <- x[-c(1:3), ]
    attributes(x)$"sim_mats_list" <- attributes(ext_sol_df)$"sim_mats_list"
    attributes(x)$"snf_config" <- attributes(ext_sol_df)$"snf_config"
    attributes(x)$"features" <- attributes(ext_sol_df)$"features"
    attributes(x)$"summary_features" <- attributes(ext_sol_df)$"summary_features"
    attributes(x)$"pvals" <- gexclude(ext_sol_df, "_pval$")
    attributes(x)$"mc_labels" <- ext_sol_df$"mc"
    x <- numcol_to_numeric(x)
    class(x) <- c("t_ext_solutions_df", "data.frame")
    x
}

#' @export
t.t_ext_solutions_df <- function(x) {
    ext_sol_df <- x
    x <- NextMethod()
    x <- data.frame(x)
    colnames(x) <- x[1, ]
    x <- x[-1, ]
    x <- numcol_to_numeric(x) |>
        dplyr::mutate(
            dplyr::across(
                dplyr::starts_with("uid_"), ~ as.integer(.)
            )
        )
    x$"solution" <- as.integer(as.numeric(sub("s", "", rownames(x))))
    x$"nclust" <- x |>
        drop_cols("solution") |>
        apply(1, function(y) length(unique(y))) |>
        as.integer()
    x$"mc" <- as.character(attributes(ext_sol_df)$"mc_labels")
    x <- cbind(x, attributes(ext_sol_df)$"pvals")
    if (!is.null(attributes(ext_sol_df)$"summary_features")) {
        summary_cols <- c("min_pval", "mean_pval", "max_pval")
    } else {
        summary_cols <- NULL
    }
    x <- x[, unique(c("solution", "nclust", "mc", summary_cols, colnames(x)))]
    attributes(x)$"snf_config" <- attributes(ext_sol_df)$"snf_config"
    attributes(x)$"features" <- attributes(ext_sol_df)$"features"
    attributes(x)$"summary_features" <- attributes(ext_sol_df)$"summary_features"
    rownames(x) <- NULL
    class(x) <- c("ext_solutions_df", "data.frame")
    x
}
