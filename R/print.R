#' Print method for class `ari_matrix`
#'
#' Custom formatted print for weights matrices that outputs
#' information about feature weights functions to the console.
#'
#' @param x A `ari_matrix` class object.
#' @param ... Other arguments passed to `print` (not used in this function)
#' @return Function prints to console but does not return any value.
#' @export
print.ari_matrix <- function(x, ...) {
    output_matrix <- x
    attributes(output_matrix)$"order" <- NULL
    class(output_matrix) <- c("matrix", "array")
    output <- utils::capture.output(print(output_matrix))
    cat(cli::col_grey("ARI matrix for ", nrow(x), " cluster solutions.\n"))
    cat(output, sep = "\n")
    cat("ARI-based order:", attributes(x)$"order", "\n")
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
            drop_cols("uid") |>
            dplyr::glimpse() |>
            utils::capture.output()
        data_main <- data_out[-c(1:2)]
        if (length(data_main) <= 5) {
            cat(data_main, sep = "\n")
        } else {
            cat(data_main[1:5], sep = "\n")
            n_more_fts <- length(data_main) - 5
            grammar <- if (n_more_fts > 1) "s.\n" else ".\n"
            cat(
                cli::col_grey(
                    "\u2026and ", n_more_fts, " more feature", grammar
                )
            )
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
                    "\u2026and ", n_more_fns, " more function", grammar
                )
            )
        } else if (length(all_output) == 0){
        } else {
            cat(cli::col_green(all_output), sep = "\n")
        }
    }
}

#' Print method for class `ext_solutions_df`
#'
#' Custom formatted print for extended solutions data frame class objects.
#'
#' @param x A `ext_solutions_df` class object.
#' @inheritParams print.solutions_df
#' @return Function prints to console but does not return any value.
#' @export
print.ext_solutions_df <- function(x, n = NULL, ...) {
    if (nrow(x) > 10 & is.null(n)) {
        n <- 10
    }
    cat(
        cli::col_grey(
            nrow(x), " cluster solutions, ", length(uids(x)), " observations,",
            " and p-values for ", length(features(x)), " features.\n"
        )
    )
    # Establishing column names for the different parts to print
    key_cols <- c("solution", "nclust", "mc")
    uid_sol_df_columns <- grep("^uid", names(x), value = TRUE)
    all_columns <- c(key_cols, uid_sol_df_columns)
    sol_df <- x[, all_columns]
    pval_df <- x[ , c("solution", grep("_pval$", names(x), value = TRUE))]
    if ("min_pval" %in% colnames(pval_df)) {
        summary_df <- pick_cols(pval_df, c("solution", "min_pval", "mean_pval", "max_pval"))
        pval_df <- drop_cols(pval_df, c("min_pval", "mean_pval", "max_pval"))
    } else {
        summary_df <- NULL
    }
    #--------------------------------------------------------------------------
    # Cluster assignment columns
    cat(cli::col_cyan("Cluster assignment columns:\n"))
    class(sol_df) <- c("solutions_df", "data.frame")
    print(sol_df, n = n, tips = FALSE)
    #--------------------------------------------------------------------------
    # Association p-value columns
    cat(cli::col_cyan("Association p-value columns:\n"))
    assignment_df <- pval_df
    assignment_df <- assignment_df |>
        tibble::tibble() |>
        dplyr::mutate(
            dplyr::across(
                .cols = dplyr::where(is.numeric) & !"solution",
                .fns = ~ formatC(.x, digits = 4, format = "e")
            )
        )
    old <- options(
        pillar.max_chars = 4,
        pillar.bold = FALSE
    )
    output <- utils::capture.output(print(assignment_df, n = n))
    options(old)
    output <- output[-c(1, 3)]
    output <- output[!grepl("^#", output)]
    # Managing spacing for different row numbers
    if (length(output) >= 1001) {
        output <- sub(".....", "", output)
    } else if (length(output) >= 101) {
        output <- sub("....", "", output)
    } else if (length(output) >= 11) {
        output <- sub("...", "", output)
    } else {
        output <- sub("..", "", output)
    }
    first_line <- output[1]
    nclust_idx <- regexpr("nclust", first_line)[1]
    mc_idx <- regexpr("mc", first_line)[1]
    substring_after_mc <- substr(first_line, mc_idx + 2, nchar(first_line))
    rest_idx <- mc_idx + nchar("mc")
    for (i in seq_along(output)) {
        sentence <- output[i]
        if (i > 1) {
            sentence <- gsub("e", cli::col_grey("e"), sentence)
            sentence <- gsub("-", cli::col_red("-"), sentence)
        }
        first <- substr(sentence, 1, 9)
        rest <- substr(sentence, 10, nchar(sentence))
        cat(
            cli::col_green(first),
            rest, "\n", sep = ""
        )
    }
    displayed_cols <- length(strsplit(output[1], "\\s+")[[1]])
    displayed_solutions <- length(output) - 1
    hidden_features <- length(features(x)) - displayed_cols + 1
    hidden_solutions <- nrow(x) - displayed_solutions
    solution_suffix <- if (hidden_solutions == 1) "" else "s"
    column_suffix <- if (hidden_features == 1) "" else "s"
    #--------------------------------------------------------------------------
    # Summary p-value columns
    if (!is.null(summary_df)) {
        summary_df <- summary_df |>
            tibble::tibble() |>
            dplyr::mutate(
                dplyr::across(
                    .cols = dplyr::where(is.numeric) & !"solution",
                    .fns = ~ formatC(.x, digits = 3, format = "e")
                )
            )
        cat(cli::col_cyan("Summary p-value columns:\n"))
        old <- options(
            pillar.max_chars = 4,
            pillar.bold = FALSE
        )
        output <- utils::capture.output(print(summary_df, n = n))
        options(old)
        output <- output[-c(1, 3)]
        output <- output[!grepl("^#", output)]
        # Managing spacing for different row numbers
        if (length(output) >= 1001) {
            output <- sub(".....", "", output)
        } else if (length(output) >= 101) {
            output <- sub("....", "", output)
        } else if (length(output) >= 11) {
            output <- sub("...", "", output)
        } else {
            output <- sub("..", "", output)
        }
        for (i in seq_along(output)) {
            sentence <- output[i]
            if (i > 1) {
                sentence <- gsub("e", cli::col_grey("e"), sentence)
                sentence <- gsub("-", cli::col_red("-"), sentence)
            }
            first <- substr(sentence, 1, 8)
            rest <- substr(sentence, 10, nchar(sentence))
            cat(cli::col_green(first), rest, "\n", sep = " ")
            summary_fts <- attributes(x)$"summary_features"
            n_sum_fts <- length(summary_fts)
        }
        cat(
            cli::col_grey(
                "Summaries calculated from ", n_sum_fts, " features.",
                " Use `summary_features(x)` to see them.\n"
            )
        )
    }
    not_shown_message(hidden_solutions, NULL, hidden_features) 
    print_with_n_message()
    print_with_t_message()
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
    comp_out <- gsub(" 1 ", cli::col_green(" ", cli::symbol$tick, " "), comp_out)
    comp_out <- gsub(" 0 ", cli::col_red(" ", cli::symbol$cross, " "), comp_out)
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

#' Print method for class `sim_mats_list`
#'
#' Custom formatted print for similarity matrix list
#'
#' @param x A `sim_mats_list` class object.
#' @param ... Other arguments passed to `print` (not used in this function).
#' @export
print.sim_mats_list <- function(x, ...) {
    if (is.null(x[[1]])) {
        cat(cli::col_grey("An empty `sim_mats_list`.\n"))
    } else {
        end <- if (length(x) != 1) "matrices.\n" else "matrix.\n"
        cat(
            cli::col_grey(
                "A similarity matrix list storing ", length(x), " ",
                nrow(x[[1]]), "x", nrow(x[[1]]), " similarity ", end
            )
        )
        cat(
            cli::col_grey(
                "Use `sim_mats_list[[i]]` to view the ith matrix.\n"
            )
        )
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
    cat(cli::col_blue("Settings Data Frame:\n"))
    print(x$"settings_df")
    cat(cli::col_blue("Distance Functions List:\n"))
    print(x$"dist_fns_list")
    cat(cli::col_blue("Clustering Functions List:\n"))
    print(x$"clust_fns_list")
    cat(cli::col_blue("Weights Matrix:\n"))
    print(x$"weights_matrix")
}

#' Print method for class `solutions_df`
#'
#' Custom formatted print for weights matrices that outputs
#' information about feature weights functions to the console.
#'
#' @param x A `weights_matrix` class object.
#' @param n Number of rows to print, passed into `tibble::print.tbl_df()`.
#' @param tips If TRUE, include lines on how to print more rows / transposed.
#' @param ... Other arguments passed to `print` (not used in this function).
#' @return Function prints to console but does not return any value.
#' @export
print.solutions_df <- function(x, n = NULL, tips = TRUE, ...) {
    if (tips) {
        if (nrow(x) == 1) {
            segment <- " cluster solution of "
        } else {
            segment <- " cluster solutions of "
        }
        cat(cli::col_grey(nrow(x), segment, length(uids(x)), " observations:\n"))
    }
    assignment_df <- tibble::tibble(as.data.frame(x))
    if (nrow(assignment_df) > 10 & is.null(n)) {
        n <- 10
    }
    output <- utils::capture.output(print(assignment_df, n = n))
    output <- output[-c(1, 3)]
    output <- output[!grepl("^#", output)]
    # Managing spacing for different row numbers
    if (length(output) >= 1001) {
        output <- sub(".....", "", output)
    } else if (length(output) >= 101) {
        output <- sub("....", "", output)
    } else if (length(output) >= 11) {
        output <- sub("...", "", output)
    } else {
        output <- sub("..", "", output)
    }
    first_line <- output[1]
    nclust_idx <- regexpr("nclust", first_line)[1]
    mc_idx <- regexpr("mc", first_line)[1]
    uid_idx <- regexpr("uid", first_line)[1]
    for (i in seq_along(output)) {
        sentence <- output[i]
        if (i > 1) {
            sentence <- gsub("<NA>", " .  ", sentence)
        }
        first <- substr(sentence, 1, 8)
        second <- substr(sentence, nclust_idx, nclust_idx + 5)
        third <- substr(sentence, mc_idx, mc_idx + 1)
        rest <- substr(sentence, uid_idx, nchar(sentence))
        cat(
            cli::col_green(first),
            cli::col_yellow(second),
            cli::col_blue(third),
            rest, "\n", sep = " "
        )
    }
    if (tips) {
        displayed_observations <- length(strsplit(output[1], "\\s+")[[1]]) - 1
        displayed_solutions <- length(output) - 1
        hidden_observations <- ncol(x) - 1 - displayed_observations
        hidden_solutions <- nrow(x) - displayed_solutions
        solution_suffix <- if (hidden_solutions == 1) "" else "s"
        observation_suffix <- if (hidden_observations == 1) "" else "s"
        not_shown_message(hidden_solutions, hidden_observations)
        print_with_n_message()
        print_with_t_message()
    }
}

#' Print method for class `t_ext_solutions_df`
#'
#' Custom formatted print for transposed solutions data frame class objects.
#'
#' @param x A `t_solutions_df` class object.
#' @param ... Other arguments passed to `print` (not used in this function)
#' @return Function prints to console but does not return any value.
#' @export
print.t_ext_solutions_df <- function(x, ...) {
    x <- tibble::tibble(data.frame(x))
    n_sols <- ncol(x) - 1
    n_obs <- nrow(x)
    output <- utils::capture.output(print(x, width = Inf))
    output <- output[!grepl("^#", output)]
    output <- sub("...", "", output)
    output <- output[!grepl("^<", output)]
    header <- output[1]
    rest <- output[-1]
    cat(cli::col_blue(header), "\n")
    # Calculating shown and hidden solutions
    shown_sols <- length(strsplit(header, "\\s+")[[1]]) - 1
    hidden_sols <- n_sols - shown_sols
    if (hidden_sols == 1) {
        sols_message <- "1 solution"
    } else if (hidden_sols > 1) {
        sols_message <- paste0(hidden_sols, " solutions")
    } else {
        sols_message <- ""
    }
    # Calculating shown and hidden observations
    if (length(rest) > 10) {
        hidden_obs <- n_obs - 10
        if (hidden_obs == 1) {
            obs_message <- "1 observation"
        } else if (hidden_obs > 1) {
            obs_message <- paste0(hidden_obs, " observations")
        }
    } else {
        hidden_obs <- 0
        obs_message <- ""
    }
    if (hidden_sols > 0 & hidden_obs > 0) {
        joiner <- " and "
    } else {
        joiner <- ""
    }
    if (hidden_obs > 0 | hidden_sols > 0) {
        cat(rest[1:10], sep = "\n")
        cat(
            cli::col_grey(
                "Not showing ", obs_message, joiner, sols_message, ".\n",
                sep = ""
            )
        )
    } else {
        cat(rest, sep = "\n")
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
    if (ncol(x) == 2) {
        segment <- " cluster solution of "
    } else {
        segment <- " cluster solutions of "
    }
    cat(cli::col_grey(ncol(x) - 1, segment, nrow(x), " observations:\n"))
    x <- tibble::tibble(data.frame(x))
    n_sols <- ncol(x) - 1
    n_obs <- nrow(x)
    x2 <- x
    spaces <- max(max(nchar(x$"uid")), 3)
    colnames(x2)[1] <- paste0(paste0(rep(" ", spaces - 3), collapse = ""), "uid")
    output <- utils::capture.output(print(x2))
    output <- output[!grepl("^#", output)]
    if (length(output) >= 1001) {
        output <- sub(".....", "", output)
    } else if (length(output) >= 101) {
        output <- sub("....", "", output)
    } else if (length(output) >= 11) {
        output <- sub("...", "", output)
    } else {
        output <- sub("..", "", output)
    }
    output <- output[!grepl("^<", output)]
    header <- sub("`", "", output[1])
    header <- sub("`", "  ", header)
    rest <- output[-1]
    cat(cli::col_blue(header), "\n")
    # Calculating shown and hidden solutions
    shown_sols <- sum(startsWith(strsplit(header, "\\s+")[[1]], "s"))
    hidden_sols <- n_sols - shown_sols
    hidden_obs <- nrow(x) - length(rest)
    cat(rest, sep = "\n")
    not_shown_message(hidden_sols, hidden_obs, NULL) 
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
    class(x) <- c("matrix", "array")
    all_output <- x |>
        data.frame() |>
        dplyr::glimpse() |>
        utils::capture.output()
    all_output <- all_output[-c(1:2)]
    all_output <- gsub("<dbl>", "", all_output)
    cat(cli::col_grey("Weights defined for ", nrow(x), " cluster solutions."))
    cat("\n")
    if (length(all_output) >= 5) {
        for (string in all_output[1:5]) {
            word_vec <- strsplit(string, "\\s+")[[1]]
            cat(word_vec, "\n")
        }
        n_more_fts <- length(all_output) - 5
        grammar <- if (n_more_fts > 1) "s.\n" else ".\n"
        if (n_more_fts > 0) {
            cat(
                cli::col_grey(
                    "\u2026and ", n_more_fts, " more feature", grammar
                )
            )
        }
    } else if (length(all_output) == 0){
    } else {
        cat(all_output, sep = "\n")
    }
}

#' Helper function for creating what hidden ft/obs/sols message
#'
#' @keywords internal
#' @param hidden_solutions Number of hidden solutions.
#' @param hidden_observations Number of hidden observations.
#' @param hidden_features Number of hidden features.
#' @return If all arguments are NULL or 0, returns NULL. Otherwise, output a
#'  neatly formatted string indicating how many observations, features, and/or
#'  observations were not shown.
not_shown_message <- function(hidden_solutions = NULL,
                              hidden_observations = NULL,
                              hidden_features = NULL) {
    input_list <- list(
        hidden_solutions,
        hidden_observations,
        hidden_features
    )
    names_list <- c("solution", "observation", "feature")
    message_parts <- mapply(
        function(x, name) {
            if (is.null(x) || x == 0) {
                return(NULL)
            }
            if (x == 1) {
                return(paste("1", name))
            } else {
                return(paste0(x, " ", name, "s"))
            }
        },
        input_list,
        names_list
    )
    message_parts <- Filter(Negate(is.null), message_parts)
    if (length(message_parts) == 0) {
        return(NULL)
    }
    # Join message parts with commas and "and"n
    if (length(message_parts) > 1) {
        message <- paste(
            paste(
                message_parts[-length(message_parts)], collapse = ", "
            ), 
            "and",
            message_parts[length(message_parts)], 
            "not shown."
        )
    } else {
        message <- paste(message_parts, "not shown.")
    }
    message <- cli::col_grey(message, "\n")
    if (!is.null(message)) {
        cat(message)
    }
}

#' Helper function for outputting tip on changing rows printed
#'
#' @keywords internal
#' @return Output a message to use print with `n` to change displayed rows.
print_with_n_message <- function() {
    cat(
        cli::col_grey(
            "Use `print(n = ...)` to change the number of rows printed.\n"
        )
    )
}

#' Helper function for transposing solutions_df message
#'
#' @keywords internal
#' @return Output a message to use print with `n` to change displayed rows.
print_with_t_message <- function() {
    cat(
        cli::col_grey(
            "Use `t()` to view compact cluster solution format.\n"
        )
    )
}
