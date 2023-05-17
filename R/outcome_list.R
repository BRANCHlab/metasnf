#' Generate outcome_list object - but softcoded
#'
#' The major object containing all outcome variables
#'
#' @param ... Lists of outcomes formatted as (dataframe, "name", "type")
#'
#' @return outcome_list structure containing all outcome measure data
#'
#' @export
generate_outcome_list <- function(...) {
    # The object that will contain all the data
    outcome_list <- list(...)
    # Assign names to the nested list elements
    outcome_list_names <- c("data", "name", "type")
    outcome_list <- lapply(outcome_list, stats::setNames, outcome_list_names)
    return(outcome_list)
}

#' Summarize an outcome list
#'
#' @param outcome_list an outcome_list
#'
#' @return ol_summary Summarized output
#'
#' @export
sol <- function(outcome_list) {
    ol_summary <-
        data.frame(
            name = unlist(lapply(outcome_list, function(x) x$"name")),
            type = unlist(lapply(outcome_list, function(x) x$"type")),
            length = unlist(lapply(outcome_list, function(x) dim(x$"data")[1])),
            width = unlist(lapply(outcome_list, function(x) dim(x$"data")[2])))
    return(ol_summary)
}


