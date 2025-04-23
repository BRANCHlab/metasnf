#' @export
c.ari_matrix <- function(...) {
    metasnf_error(
        "No method for `c` on `ari_matrix` class objects."
    )
}

#' @export
c.clust_fns_list <- function(...) {
    result <- NextMethod()
    class(result) <- c("clust_fns_list", "list")
    result
}

#' @export
c.dist_fns_list <- function(...) {
    metasnf_error(
        "No method for `c` on `dist_fns_list` class objects.",
        " Please use `merge()` instead."
    )
}

#' @export
c.data_list <- function(...) {
    dls <- list(...)
    dlls <- lapply(
        dls,
        function(x) {
            class(x) <- "list"
            return(x)
        }
    )
    dll <- do.call(c, dlls) |>
        remove_dll_incomplete() |>
        arrange_dll() |>
        dll_uid_first_col()
    validate_data_list(dll)
    dl <- new_data_list(dll)
    return(dl)
}

#' @export
c.ext_solutions_df <- function(...) {
    metasnf_error(
        "No method for `c` on `ext_solutions_df` class objects.",
        " To combine multiple solutions data frames, use `rbind()` instead."
    )
}

#' @export
c.settings_df <- function(...) {
    metasnf_error(
        "No method for `c` on `settings_df` class objects.",
        " To combine multiple solutions data frames, use `rbind()` instead."
    )
}

#' @export
c.sim_mats_list <- function(...) {
    smll <- NextMethod()
    smll <- validate_sim_mats_list(smll)
    sml <- as_sim_mats_list(smll)
    return(sml)
}

#' @export
c.snf_config <- function(...) {
    metasnf_error(
        "No method for `c` on `snf_config` class objects.",
        " To combine multiple solutions data frames, use `rbind()` instead."
    )
}


#' @export
c.solutions_df <- function(...) {
    metasnf_error(
        "No method for `c` on `solutions_df` class objects.",
        " To combine multiple solutions data frames, use `rbind()` instead."
    )
}

#' @export
c.t_ext_solutions_df <- function(...) {
    metasnf_error(
        "No method for `c` on `t_ext_solutions_df` class objects.",
        " To combine multiple solutions data frames, use `rbind()` instead."
    )
}

#' @export
c.t_solutions_df <- function(...) {
    metasnf_error(
        "No method for `c` on `t_solutions_df` class objects.", 
        " To combine multiple solutions data frames, use `rbind()` instead."
    )
}
