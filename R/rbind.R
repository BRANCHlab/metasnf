#' Row-binding of solutions data frame class objects.
#'
#' @param reset_indices If TRUE, re-labels the "solutions" indices in
#'  the solutions data frame from 1 to the number of defined settings.
#' @param ... An arbitrary number of `solutions_df` class objects.
#' @return A `solutions_df` class object.
#' @export
rbind.solutions_df <- function(reset_indices = FALSE, ...) {
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
            attributes(x)$"snf_config"$"weights_matrix"
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
    merged_wm <- do.call(rbind, wms)
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

#' Row-binding of solutions data frame class objects.
#'
#' @param reset_indices If TRUE, re-labels the "solutions" indices in
#'  the solutions data frame from 1 to the number of defined settings.
#' @param ... An arbitrary number of `ext_solutions_df` class objects.
#' @return An `ext_solutions_df` class object.
#' @export
rbind.ext_solutions_df <- function(reset_indices = FALSE, ...) {
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
            attributes(x)$"snf_config"$"weights_matrix"
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
    merged_wm <- do.call(rbind, wms)
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

#' @export
rbind.weights_matrix <- function(...) {
    result <- as.matrix(rbind.data.frame(...))
    result <- validate_weights_matrix(result)
    result <- new_weights_matrix(result)
    return(result)
}
