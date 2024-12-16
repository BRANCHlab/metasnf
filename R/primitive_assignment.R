#' Assignment operator for data lists
#'
#' Enables usage of `[<-` assignment operator on `data_list` class objects.
#' Given a `numeric` or `character` index, returns a subsetted data list.
#' Data lists only use single-dimension extraction (component-wise).
#'
#' @param x A `data_list` class object.
#' @param i Index for component assignment.
#' @param value A list structured as a valid component, i.e. a named list of
#'  items "data", "name", "domain", and "type" respectively of classes
#'  '`data.frame`, `character`, `charater`, `character`. UIDs in value must
#'  match those in the data list.
#' @return A data list with the assigned component.
#' @export
`[<-.data_list` <- function(x, i, value) {
    dll <- NextMethod()
    validate_data_list(dll)
    dl <- as_data_list(dll)
    return(dl)
}
