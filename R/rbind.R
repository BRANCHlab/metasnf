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
