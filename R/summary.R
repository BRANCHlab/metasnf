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
