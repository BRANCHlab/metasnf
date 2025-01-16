#' Transpose a solutions data frame
#'
#' @param x A `solutions_df` class object.
#' @return A transposed `solutions_df` class object (class data.frame).
#' @export
t.solutions_df <- function(x) {
    sol_df <- x
    x <- NextMethod()
    x <- data.frame(x)
    x$"uid" <- rownames(x)
    rownames(x) <- NULL
    x <- dplyr::select(x, "uid", dplyr::everything())
    colnames(x) <- c("uid", paste0("s", sol_df$"solution"))
    # Remove transposed solution, nclust, and mc labels
    x <- x[-c(1:3), ]
    attributes(x)$"sim_mats_list" <- attributes(sol_df)$"sim_mats_list"
    attributes(x)$"snf_config" <- attributes(sol_df)$"snf_config"
    attributes(x)$"mc_labels" <- sol_df$"mc"
    x <- numcol_to_numeric(x)
    class(x) <- c("t_solutions_df", "data.frame")
    x
}

#' Transpose a transposed solutions data frame
#'
#' @param x A `solutions_df` class object.
#' @return A transposed `solutions_df` class object (class data.frame).
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
        dplyr::select(-"solution") |>
        apply(1, function(y) length(unique(y))) |>
        as.integer()
    x$"mc" <- as.character(attributes(sol_df)$"mc_labels")
    x <- dplyr::select(
        x,
        "solution",
        "nclust",
        "mc",
        dplyr::everything()
    )
    class(x) <- c("solutions_df", "data.frame")
    attributes(x)$"sim_mats_list" <- attributes(sol_df)$"sim_mats_list"
    attributes(x)$"snf_config" <- attributes(sol_df)$"snf_config"
    rownames(x) <- NULL
    x
}

#' Transpose a solutions data frame
#'
#' @param x A `solutions_df` class object.
#' @return A transposed `solutions_df` class object (class data.frame).
#' @export
t.ext_solutions_df <- function(x) {
    ext_sol_df <- x
    x <- dplyr::select(x, -dplyr::ends_with("_pval"))
    x <- NextMethod()
    x <- data.frame(x)
    x$"uid" <- rownames(x)
    rownames(x) <- NULL
    x <- dplyr::select(x, "uid", dplyr::everything())
    colnames(x) <- c("uid", paste0("s", ext_sol_df$"solution"))
    x <- x[-c(1:3), ]
    attributes(x)$"sim_mats_list" <- attributes(ext_sol_df)$"sim_mats_list"
    attributes(x)$"snf_config" <- attributes(ext_sol_df)$"snf_config"
    attributes(x)$"features" <- attributes(ext_sol_df)$"features"
    attributes(x)$"pvals" <- dplyr::select(ext_sol_df, dplyr::ends_with("_pval"))
    attributes(x)$"mc_labels" <- sol_df$"mc"
    x <- numcol_to_numeric(x)
    class(x) <- c("t_ext_solutions_df", "data.frame")
    x
}

#' Transpose a transposed solutions data frame
#'
#' @param x A `solutions_df` class object.
#' @return A transposed `solutions_df` class object (class data.frame).
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
        dplyr::select(-"solution") |>
        apply(1, function(y) length(unique(y))) |>
        as.integer()
    x$"mc" <- as.character(attributes(ext_sol_df)$"mc_labels")
    x <- dplyr::select(
        x,
        "solution",
        "nclust",
        "mc",
        dplyr::everything()
    )
    x <- cbind(x, attributes(ext_sol_df)$"pvals")
    attributes(x)$"sim_mats_list" <- attributes(ext_sol_df)$"sim_mats_list"
    attributes(x)$"snf_config" <- attributes(ext_sol_df)$"snf_config"
    attributes(x)$"features" <- attributes(ext_sol_df)$"features"
    rownames(x) <- NULL
    class(x) <- c("ext_solutions_df", "data.frame")
    x
}
