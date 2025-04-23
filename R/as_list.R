#' Coerce a `clust_fns_list` class object into a `list` class object
#'
#' @param x A `clust_fns_list` class object.
#' @param ... Additional parameter passed to `as.list()`.
#' @return A `list` class object with all the functions of `x`.
#' @export
as.list.clust_fns_list <- function(x,
                                  ...) {
    class(x) <- "list"
    return(x)
}

#' Coerce a `data_list` class object into a `list` class object
#'
#' @param x A `data_list` class object.
#' @param ... Additional parameter passed to `as.list()`.
#' @return A `list` class object with all the objects of `x`.
#' @export
as.list.data_list <- function(x,
                              ...) {
    class(x) <- "list"
    return(x)
}

#' Coerce a `dist_fns_list` class object into a `list` class object
#'
#' @param x A `dist_fns_list` class object.
#' @param ... Additional parameter passed to `as.list()`.
#' @return A `list` class object with all the functions of `x`.
#' @export
as.list.dist_fns_list <- function(x,
                                  ...) {
    class(x) <- "list"
    return(x)
}

#' Coerce a `sim_mats_list` class object into a `list` class object
#'
#' @param x A `sim_mats_list` class object.
#' @param ... Additional parameter passed to `as.list()`.
#' @return A `list` class object with all the functions of `x`.
#' @export
as.list.sim_mats_list <- function(x,
                                  ...) {
    class(x) <- "list"
    return(x)
}

#' Coerce a `snf_config` class object into a `list` class object
#'
#' @param x A `snf_config` class object.
#' @param ... Additional parameter passed to `as.list()`.
#' @return A `list` class object with all the functions of `x`.
#' @export
as.list.snf_config <- function(x,
                                  ...) {
    class(x) <- "list"
    return(x)
}
