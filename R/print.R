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
    cat(
        cli::col_yellow(
            "Components (c = ", n_components, ", p = ", n_features, "):\n"
        )
    )
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

#' Print method for class `snf_config`
#'
#' Custom formatted print for SNF config
#'
#' @param x A `snf_config` class object.
#' @param ... Other arguments passed to `print` (not used in this function)
#' @return Function prints to console but does not return any value.
#' @export
print.snf_config <- function(x, ...) {
    cat(cli::col_blue("Settings:\n"))
    print(x$"settings_df")
    cat(cli::col_blue("Distance Functions List:\n"))
    print(x$"dist_fns_list")
    cat(cli::col_blue("Clustering functions:\n"))
    print(x$"clust_fns_list")
    cat(cli::col_blue("Weights:\n"))
    print(x$"weights_matrix")
}

#' Print method for class `snf_results`
#'
#' Custom formatted print for SNF results
#'
#' @param x A `snf_results` class object.
#' @param ... Other arguments passed to `print` (not used in this function)
#' @return Function prints to console but does not return any value.
#' @export
print.snf_results <- function(x, ...) {
    NextMethod()
}

#' Print method for class `settings_df`
#'
#' Custom formatted print for settings data frame that outputs information
#' about SNF hyperparameters to the console.
#'
#' @param x A `settings_df` class object.
#' @param ... Other arguments passed to `print` (not used in this function)
#' @return Function prints to console but does not return any value.
#' @export
print.settings_df <- function(x, ...) {
    # Settings DF includes 11 boilerplate columns
    BOILERPLATE_COLS <- 11
    # Number of components is found by subtracting off boilerplate columns
    n_comp <- ncol(x) - BOILERPLATE_COLS
    # String for manipulation into each piece of output
    all_output <- utils::capture.output(t(x))
    # The corresponding row index
    idx_out <- all_output[1]
    idx_out <- gsub("\\[,", "  ", idx_out)
    idx_out <- gsub("\\]", " ", idx_out)
    idx_out <- sub("  ", "", idx_out)
    hyper_out <- all_output[c(3:5)]
    hyper_out <- gsub("\\.0", "  ", hyper_out)
    hyper_out <- sub("alpha  ", "alpha", hyper_out)
    scheme_out <- all_output[6]
    scheme_out <- gsub("\\.0", "  ", scheme_out)
    scheme_out <- sub("snf_scheme", "          ", scheme_out)
    clust_out <- all_output[7]
    clust_out <- gsub("\\.0", "  ", clust_out)
    clust_out <- sub("clust_alg", "         ", clust_out)
    dist_out <- all_output[8:12]
    dist_out <- gsub("\\.0", "  ", dist_out)
    dist_out <- sub("_dist", "     ", dist_out)
    dist_out <- toupper(dist_out)
    comp_out <- all_output[13:(12 + n_comp)]
    comp_out <- gsub("\\.0", "  ", comp_out)
    comp_out <- sub("inc_([^ ]+)", "\\1    ", comp_out)
    comp_out <- gsub("1", cli::col_green(cli::symbol$tick), comp_out)
    comp_out <- gsub("0", cli::col_red(cli::symbol$cross), comp_out)
    if (nrow(x) == 0) {
        scheme_out <- cli::col_grey("empty")
        clust_out <- cli::col_grey("empty")
        hyper_out <- cli::col_grey("empty")
        dist_out <- cli::col_grey("empty")
        comp_out <- cli::col_grey("empty")
    } else {
        cat(cli::col_silver(idx_out), sep = "\n")
    }
    cat(cli::col_yellow("SNF hyperparameters:"), hyper_out, sep = "\n")
    cat(cli::col_yellow("SNF scheme:"), scheme_out, sep = "\n")
    cat(cli::col_yellow("Clustering functions:"), clust_out, sep = "\n")
    cat(cli::col_yellow("Distance functions:"), dist_out, sep = "\n")
    cat(cli::col_yellow("Component dropout:"), comp_out, sep = "\n")
    # Message for number of rows not shown 
    if (nrow(x) == 0) {
        shown_idx <- 0
    } else {
        shown_idx <- max(
            stats::na.omit(as.numeric(strsplit(idx_out, "\\s+")[[1]]))
        )
    }
    hidden_idx <- nrow(x) - shown_idx
    if (hidden_idx > 0) {
        grammar <- if (hidden_idx > 1) "s.\n" else ".\n"
        cat(
            cli::col_grey(
                "\u2026and settings defined to create ",
                hidden_idx,
                " more cluster solution",
                grammar
            ),
            sep = ""
        )
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
                    "\u2026and ", n_more_fns, " more function", grammar
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
        cat(cli::col_grey("\u2026and ", n_more_fns, " more function", grammar))
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
        utils::capture.output()
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

#' Print method for class `weights_matrix`
#'
#' Custom formatted print for weights matrices that outputs
#' information about feature weights functions to the console.
#'
#' @param x A `weights_matrix` class object.
#' @param ... Other arguments passed to `print` (not used in this function)
#' @return Function prints to console but does not return any value.
#' @export
print.solutions_df <- function(x, ...) {
    cat(
        cli::col_grey(
            nrow(x), " cluster solutions of ", ncol(x) - 2, " observations:\n"
        )
    )
    assignment_df <- tibble::tibble(data.frame(x))
    output <- utils::capture.output(print(assignment_df))
    output <- output[-c(1, 3)]
    output <- output[!grepl("^#", output)]
    output <- sub("...", "", output)
    for (sentence in output) {
        first <- substr(sentence, 1, 8)
        second <- substr(sentence, 9, 16)
        rest <- substr(sentence, 17, nchar(sentence))
        cat(
            cli::col_green(first),
            cli::col_yellow(second),
            rest, "\n", sep = ""
        )
    }
    displayed_observations <- length(strsplit(output[1], "\\s+")[[1]]) - 1
    displayed_solutions <- length(output) - 1
    hidden_observations <- ncol(x) - 1 - displayed_observations
    hidden_solutions <- nrow(x) - displayed_solutions
    solution_suffix <- if (hidden_solutions == 1) "" else "s"
    observation_suffix <- if (hidden_observations == 1) "" else "s"
    if (hidden_solutions > 0 & hidden_observations > 0) {
        cat(
            cli::col_grey(
                hidden_solutions, " solution",
                solution_suffix, " and ", hidden_observations,
                " observation", observation_suffix, " not shown.\n"
            )
        )
    } else if (hidden_solutions > 0) {
        cat(
            cli::col_grey(
                hidden_solutions, " solution",
                solution_suffix, " not shown.\n"
            )
        )
    } else if (hidden_observations > 0) {
        cat(
            cli::col_grey(
                hidden_observations, " observation",
                observation_suffix, " not shown.\n"
            )
        )
    }
}

#' Print method for class `t_solutions_df`
#'
#' Custom formatted print for transposed solutions data frame class objects.
#'
#' @param x A `t_solutions_df` class object.
#' @param ... Other arguments passed to `print` (not used in this function)
#' @return Function prints to console but does not return any value.
#' @export
print.t_solutions_df <- function(x, ...) {
    x <- tibble::tibble(data.frame(x))
    output <- utils::capture.output(x)
    output <- output[!grepl("^#", output)]
    output <- sub("...", "", output)
    output <- output[!grepl("^<", output)]
    cat(cli::col_blue(output[1]), "\n")
    cat(output[-1], sep = "\n")
}
