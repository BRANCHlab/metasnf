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

#' Assignment operator for settings data frames
#'
#' Enables usage of `[<-` assignment operator on `settings_df` class objects.
#' Given a `numeric` or `character` index, returns an new settings data frame.
#'
#' @param x A `settings_df` class object.
#' @param i Index for component assignment.
#' @param j Index for component assignment.
#' @param value Value to pass into x.
#' @return A settings data frame with the assigned value(s).
#' @export
`[<-.settings_df` <- function(x, i, j, value) {
    result <- NextMethod()
    class(result) <- setdiff(class(result), "settings_df")
    result <- tryCatch(
        expr = {
            result <- validate_settings_df(result)
            result <- new_settings_df(result)
            result
        },
        error = function(e) {
            result
        }
    )
    return(result)
}

#' Assignment operator for solutions data frame
#'
#' Enables usage of `[<-` assignment operator on `solutions_df` objects.
#' Given a `numeric` or `character` index, returns an new settings data frame.
#'
#' @param x A `solutions_df` class object.
#' @param i Index for component assignment.
#' @param j Index for component assignment.
#' @param value Value to pass into x.
#' @return A settings data frame with the assigned value(s).
#' @export
`[<-.solutions_df` <- function(x, i, j, value) {
    result <- NextMethod()
    class(result) <- setdiff(class(result), "solutions_df")
    result <- tryCatch(
        expr = {
            result <- validate_solutions_df(result)
            result <- new_solutions_df(result)
            result
        },
        error = function(e) {
            result
        }
    )
    return(result)
}

#' Assignment operator for extended solutions data frame
#'
#' Enables usage of `[<-` assignment operator on `ext_solutions_df` objects.
#' Given a `numeric` or `character` index, returns an new settings data frame.
#'
#' @param x A `ext_solutions_df` class object.
#' @param i Index for component assignment.
#' @param j Index for component assignment.
#' @param value Value to pass into x.
#' @return A settings data frame with the assigned value(s).
#' @export
`[<-.ext_solutions_df` <- function(x, i, j, value) {
    result <- NextMethod()
    class(result) <- setdiff(class(result), "ext_solutions_df")
    result <- tryCatch(
        expr = {
            result <- validate_ext_solutions_df(result)
            result <- new_ext_solutions_df(result)
            result
        },
        error = function(e) {
            result
        }
    )
    return(result)
}

#' Assignment operator for weights matrix
#'
#' Enables usage of `[<-` assignment operator on `weights_matrix` objects.
#' Given a `numeric` or `character` index, returns an new settings data frame.
#'
#' @param x A `weights_matrix` class object.
#' @param i Index for component assignment.
#' @param j Index for component assignment.
#' @param value Value to pass into x.
#' @return A weights matrix with the assigned value(s).
#' @export
`[<-.weights_matrix` <- function(x, i, j, value) {
    result <- NextMethod()
    class(result) <- setdiff(class(result), "weights_matrix")
    result <- tryCatch(
        expr = {
            result <- validate_weights_matrix(result)
            result <- new_weights_matrix(result)
            result
        },
        error = function(e) {
            result
        }
    )
    return(result)
}

`[<-.sim_mats_list` <- function(x, i, value) {
    cat("cake")
    NextMethod()
}
