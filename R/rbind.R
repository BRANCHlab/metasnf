#' Row-binding of extended solutions data frame class objects.
#'
#' @param ... An arbitrary number of `ext_solutions_df` class objects.
#' @return An `ext_solutions_df` class object.
#' @export
rbind.ext_solutions_df <- function(...) {
    args <- list(...)   
    all_ext_sol_dfs <- lapply(
        args,
        function(x) {
            inherits(x, "ext_solutions_df")
        }
    ) |>
        unlist() |>
        all()
    if (!all_ext_sol_dfs) {
        metasnf_error(
            "`rbind` cannot be applied to mixed ext_solutions_df and other",
            " object types."
        )
    }
    sol_dfs <- lapply(
        args,
        function(x) {
            attributes(x)$"solutions_df"
        }
    )
    result <- rbind.data.frame(...)
    attributes(result)$"solutions_df" <- do.call(rbind, sol_dfs)
    return(result)
}

#' Row-binding of solutions data frame class objects.
#'
#' @param ... An arbitrary number of `solutions_df` class objects.
#' @return A `solutions_df` class object.
#' @export
rbind.solutions_df <- function(...) {
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
    metasnf_alert("Resettings solutions indices during `rbind`.")
    result <- rbind.data.frame(...)
    result$"solution" <- seq_len(nrow(result))
    merged_sdf <- do.call(rbind, sdfs)
    merged_sdf$"solution" <- seq_len(nrow(result))
    merged_wm <- do.call(rbind, wms)
    merged_sml <- do.call(c, smls)
    attributes(result)$"sim_mats_list" <- merged_sml
    attributes(result)$"snf_config"$"settings_df" <- merged_sdf
    attributes(result)$"snf_config"$"weights_matrix" <- as_weights_matrix(merged_wm)
    return(result)
}
