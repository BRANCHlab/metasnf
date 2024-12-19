#' Print method for class `data_list`
#'
#' Custom formatted print for data list objects that outputs information about
#' the contained observations and components to the console.
#'
#' @param x A `data_list` class object.
#' @param ... Other arguments passed to `print` (not used in this function)
#' @return Function prints to console but does not return any value.
#' @export
print.data_list <- function(x, ...) {
    uids <- attributes(x)$"uids"
    n_components <- attributes(x)$"n_components"
    n_features <- attributes(x)$"n_features"
    cat(cli::col_yellow("Observations (n = ", length(uids), "):\n"))
    uid_output <- utils::capture.output(cat(uids, sep = ", "))
    max_chars <- getOption("width")
    cat(substr(uid_output, 1, max_chars - 2), "\u2026\n", sep = "")
    cat(cli::col_yellow("Components (c = ", n_components, ", p = ", n_features, "):\n"))
    # Iterate over data list components
    for (i in seq_along(x)) {
        # Send metadata to console
        component <- x[[i]]
        cat(
            cli::col_green("[", i, "] ", component$"name"),
            cli::col_grey(
                " <domain: ", component$"domain",
                ", type: ", component$"type",
                ", p = ", ncol(component$"data") - 1,
                ">"
            ),
            "\n",
            sep = ""
        )
        # Capture tibble formatted data for additional manipulations
        data_out <- component$"data" |>
            dplyr::select(-"uid") |>
            dplyr::glimpse() |>
            utils::capture.output()
        data_main <- data_out[-c(1:2)]
        if (length(data_main) <= 5) {
            cat(data_main, sep = "\n")
        } else {
            cat(data_main[1:5], sep = "\n")
            n_more_fts <- length(data_main) - 5
            grammar <- if (n_more_fts > 1) "s.\n" else ".\n"
            cat(cli::col_grey("\u2026and ", n_more_fts, " more feature", grammar))
        }
    }
}

#' Print method for class `dist_fns_list`
#'
#' Custom formatted print for distance metrics list objects that outputs
#' information about the contained distance metrics to the console.
#'
#' @param x A `dist_fns_list` class object.
#' @param ... Other arguments passed to `print` (not used in this function)
#' @return Function prints to console but does not return any value.
#' @export
print.dist_fns_list <- function(x, ...) {
    dfl_names <- c(
        "Continuous" = "cnt_dist_fns",
        "Discrete" = "dsc_dist_fns",
        "Ordinal" = "ord_dist_fns",
        "Categorical" = "cat_dist_fns",
        "Mixed" = "mix_dist_fns"
    )
    for (i in seq_along(dfl_names)) {
        clean_name <- names(dfl_names)[i]
        this_type <- dfl_names[i]
        this_type_fns <- x[[this_type]]
        cat(cli::col_yellow(clean_name, " (", length(this_type_fns),   "):\n"))
        all_output <- c()
        for (fn_idx in seq_along(x[[this_type]])) {
            output <- utils::capture.output(
                cat(
                    cli::col_green(
                        "[", fn_idx, "] ", names(x[[this_type]])[fn_idx],
                        "\n",
                        sep = ""
                    )
                )
            )
            all_output <- c(all_output, output)
        }
        if (length(all_output) > 5) {
            cat(cli::col_green(all_output[1:5]), sep = "\n")
            n_more_fns <- length(all_output) - 5
            grammar <- if (n_more_fns > 1) "s.\n" else ".\n"
            cat(
                cli::col_grey(
                    "\u2026and ", n_more_fts, " more function", grammar
                )
            )
        } else if (length(all_output) == 0){
        } else {
            cat(cli::col_green(all_output), sep = "\n")
        }
    }
}

#' Print method for class `clust_fns_list`
#'
#' Custom formatted print for clustering functions list objects that outputs
#' information about the contained clustering functions to the console.
#'
#' @param x A `clust_fns_list` class object.
#' @param ... Other arguments passed to `print` (not used in this function)
#' @return Function prints to console but does not return any value.
#' @export
print.clust_fns_list <- function(x, ...) {
    all_output <- c()
    for (fn_idx in seq_along(x)) {
        output <- utils::capture.output(
            cat(
                cli::col_green(
                    "[", fn_idx, "] ", names(x)[fn_idx],
                    "\n",
                    sep = ""
                )
            )
        )
        all_output <- c(all_output, output)
    }
    if (length(all_output) > 5) {
        cat(cli::col_green(all_output[1:5]), sep = "\n")
        n_more_fns <- length(all_output) - 5
        grammar <- if (n_more_fns > 1) "s.\n" else ".\n"
        cat(cli::col_grey("\u2026and ", n_more_fts, " more function", grammar))
    } else if (length(all_output) == 0){
    } else {
        cat(cli::col_green(all_output), sep = "\n")
    }
}

#' Print method for class `weights_matrix`
#'
#' Custom formatted print for weights matrices that outputs
#' information about feature weights functions to the console.
#'
#' @param x A `weights_matrix` class object.
#' @param ... Other arguments passed to `print` (not used in this function)
#' @return Function prints to console but does not return any value.
#' @export
print.weights_matrix <- function(x, ...) {
    all_output <- x |>
        data.frame() |>
        dplyr::glimpse() |>
        capture.output()
    all_output <- all_output[-c(1:2)]
    cat(cli::col_grey("Weights defined for ", nrow(x), " cluster solutions."))
    cat("\n")
    if (length(all_output) > 5) {
        for (string in all_output[1:5]) {
            word_vec <- strsplit(string, "\\s+")[[1]]
            cat(word_vec)
            cat("\n")
        }
        n_more_fts <- length(all_output) - 5
        grammar <- if (n_more_fts > 1) "s.\n" else ".\n"
        cat(cli::col_grey("\u2026and ", n_more_fts, " more feature", grammar))
    } else if (length(all_output) == 0){
    } else {
        cat(cli::col_green(all_output), sep = "\n")
    }
}
