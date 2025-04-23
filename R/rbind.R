#' @export
rbind.ari_matrix <- function(...) {
    metasnf_error("`rbind` cannot be applied to ari_matrix class objects.")
}

#' @export
rbind.clust_fns_list <- function(...) {
    metasnf_error("`rbind` cannot be applied to clust_fns_list class objects.")
}

#' @export
rbind.dist_fns_list <- function(...) {
    metasnf_error("`rbind` cannot be applied to dist_fns_list class objects.")
}

#' @export
rbind.data_list <- function(...) {
    metasnf_error("`rbind` cannot be applied to data_list class objects.")
}

#' @export
rbind.sim_mats_list <- function(...) {
    metasnf_error("`rbind` cannot be applied to sim_mats_list class objects.")
}

#' @export
rbind.snf_config <- function(...) {
    metasnf_error("`rbind` cannot be applied to snf_config class objects.")
}

#' Row-binding of solutions data frame class objects
#'
#' @param reset_indices If TRUE, re-labels the "solutions" indices in
#'  the solutions data frame from 1 to the number of defined settings.
#' @param ... An arbitrary number of `solutions_df` class objects.
#' @return A `solutions_df` class object.
#' @export
rbind.solutions_df <- function(..., reset_indices = FALSE) {
    args <- list(...)   
    all_sol_dfs <- lapply(
        args,
        function(x) {
            inherits(x, "solutions_df")
        }
    ) |>
        unlist() |>
        all()
    if (!all_sol_dfs) {
        metasnf_error(
            "`rbind` cannot be applied to mixed solutions_df and other",
            " object types."
        )
    }
    sdfs <- lapply(
        args,
        function(x) {
            attributes(x)$"snf_config"$"settings_df"
        }
    )
    wms <- lapply(
        args,
        function(x) {
            as.matrix(
                attributes(x)$"snf_config"$"weights_matrix"
            )
        }
    )
    smls <- lapply(
        args,
        function(x) {
            attributes(x)$"sim_mats_list"
        }
    )
    result <- rbind.data.frame(...)
    merged_sdf <- do.call(rbind, sdfs)
    merged_wm <- as_weights_matrix(do.call(rbind, wms))
    merged_sml <- do.call(c, smls)
    if (reset_indices) {
        result$"solution" <- seq_len(nrow(result))
        merged_sdf$"solution" <- seq_len(nrow(merged_sdf))
    }
    attributes(result)$"sim_mats_list" <- merged_sml
    attributes(result)$"snf_config"$"settings_df" <- merged_sdf
    attributes(result)$"snf_config"$"weights_matrix" <- merged_wm
    return(result)
}

#' Row-binding of solutions data frame class objects
#'
#' @param reset_indices If TRUE, re-labels the "solutions" indices in
#'  the solutions data frame from 1 to the number of defined settings.
#' @param ... An arbitrary number of `ext_solutions_df` class objects.
#' @return An `ext_solutions_df` class object.
#' @export
rbind.ext_solutions_df <- function(..., reset_indices = FALSE) {
    args <- list(...)   
    all_sol_dfs <- lapply(
        args,
        function(x) {
            inherits(x, "ext_solutions_df")
        }
    ) |>
        unlist() |>
        all()
    if (!all_sol_dfs) {
        metasnf_error(
            "`rbind` cannot be applied to mixed ext_solutions_df and other",
            " object types."
        )
    }
    sdfs <- lapply(
        args,
        function(x) {
            attributes(x)$"snf_config"$"settings_df"
        }
    )
    wms <- lapply(
        args,
        function(x) {
            as.matrix(
                attributes(x)$"snf_config"$"weights_matrix"
            )
        }
    )
    smls <- lapply(
        args,
        function(x) {
            attributes(x)$"sim_mats_list"
        }
    )
    result <- rbind.data.frame(...)
    merged_sdf <- do.call(rbind, sdfs)
    merged_wm <- as_weights_matrix(do.call(rbind, wms))
    merged_sml <- do.call(c, smls)
    if (reset_indices) {
        result$"solution" <- seq_len(nrow(result))
        merged_sdf$"solution" <- seq_len(nrow(merged_sdf))
    }
    attributes(result)$"sim_mats_list" <- merged_sml
    attributes(result)$"snf_config"$"settings_df" <- merged_sdf
    attributes(result)$"snf_config"$"weights_matrix" <- merged_wm
    return(result)
}

#' Row-binding of t_solutions_df class objects
#'
#' Vertically stack two or more `t_solutions_df` class objects.
#'
#' @param ... An arbitrary number of `t_solutions_df` class objects.
#' @return A `t_solutions_df` class object.
#' @export
rbind.t_solutions_df <- function(...) {
    args <- list(...)
    all_t_sol_dfs <- lapply(
        args,
        function(x) {
            inherits(x, "t_solutions_df")
        }
    ) |>
        unlist() |>
        all()
    if (!all_t_sol_dfs) {
        metasnf_error(
            "`rbind` cannot be applied to mixed t_solutions_df and other",
            " object types."
        )
    }
    sol_dfs <- lapply(args, t)
    stacked_sol_df <- rbind.solutions_df(sol_dfs)
    return(t(stacked_sol_df))
}

#' Row-bind weights matrices
#'
#' Vertically stack two or more `weights_matrix` class objects.
#'
#' @param ... An arbitrary number of `weights_matrix` class objects.
#' @return A `weights_matrix` class object.
#' @export
rbind.weights_matrix <- function(...) {
    result <- as.matrix(rbind.data.frame(...))
    result <- validate_weights_matrix(result)
    result <- new_weights_matrix(result)
    return(result)
}
