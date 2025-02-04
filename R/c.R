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
c.sim_mats_list <- function(...) {
    smll <- NextMethod()
    smll <- validate_sim_mats_list(smll)
    sml <- as_sim_mats_list(smll)
    return(sml)
}
