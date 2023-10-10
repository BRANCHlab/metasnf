#' Build a settings matrix
#'
#' @param data_list a data list object to determine variables for inclusion and
#'  exclusion
#' @param nrows number of settings matrix rows
#' @param seed set a seed for the random matrix generation. Note that this
#' @param min_removed The smallest number of elements that may be removed
#' @param max_removed The largest number of elements that may be removed
#' @param retry_limit The maximum number of attempts to generate a novel row
#'  affects the global seed.
#'
#' @return settings_matrix A settings matrix
#'
#' @export
generate_settings_matrix <- function(data_list,
                                   nrows = 0,
                                   seed = NULL,
                                   min_removed = NULL,
                                   max_removed = NULL,
                                   retry_limit = 10) {
    if (!is.null(seed)) {
        set.seed(seed)
        print("The global seed has been changed!")
    }
    dm_cols <- c(
        "row_id",
        paste0("inc_", summarize_dl(data_list)$"name"),
        "snf_scheme",
        "eigen_or_rot",
        "K",
        "alpha")
    settings_matrix_base <- as.data.frame(
        matrix(
            0,
            ncol = length(dm_cols),
            nrow = 0
        )
    )
    colnames(settings_matrix_base) <- dm_cols
    settings_matrix <- add_settings_matrix_rows(
        settings_matrix = settings_matrix_base,
        nrows = nrows,
        min_removed = min_removed,
        max_removed = max_removed,
        retry_limit = retry_limit
    )
    return(settings_matrix)
}

#' Generate random removal sequence
#'
#' Helper function to contribute to rows within the settings matrix.
#' Number of columns removed follows the exponential probability distribution
#' to typically keep all or most columns.
#'
#' @param num_cols Number of feature elements in consideration for exclusion
#' @param min_removed The smallest number of elements that may be removed
#' @param max_removed The largest number of elements that may be removed
#'
#' @return shuffled_removals Binary vector sequence indicating if a column
#' should be included (1) or excluded (0)
#'
#' @export
random_removal <- function(num_cols,
                           min_removed,
                           max_removed) {
    # Generate 10,000 random numbers according to exponential distribution
    if (is.null(min_removed)) {
        min_removed <- 0
    }
    if (is.null(max_removed)) {
        max_removed <- num_cols - 1
    }
    if (max_removed >= num_cols || min_removed < 0) {
        stop(
            paste0(
                "The number of removed elements must be between 0 and the",
                " total number of elements in the data_list (", num_cols, ")."
            )
        )
    }
    rand_vals <- stats::rexp(10000)
    # Scale them to range from 0 to num_cols - 1
    # multiply by the difference
    # add the lowest value
    difference <- max_removed - min_removed
    # Scale the values to range from 0 to 1
    rand_vals <- rand_vals / max(rand_vals)
    # Multiply by the difference
    rand_vals <- rand_vals * difference
    rand_vals <- rand_vals + min_removed
    rand_vals <- round(rand_vals)
    num_removed <- sample(rand_vals, 1)
    # vector of 1s or 0s to represent number of columns kept
    remove_placeholders <- rep(0, num_removed)
    keep_placeholders <- rep(1, num_cols - num_removed)
    # merge and shuffle those vectors to produce final dropout sequence
    unshuffled_removals <- c(remove_placeholders, keep_placeholders)
    shuffled_removals <- sample(unshuffled_removals)
    return(shuffled_removals)
}

#' Add settings matrix rows
#'
#' @param settings_matrix The existing settings matrix
#' @param nrows The number of rows to be added to the settings matrix
#' @param retry_limit The maximum number of attempts to generate a novel row
#' @param min_removed The smallest number of elements that may be removed
#' @param max_removed The largest number of elements that may be removed
#'
#' @return settings_matrix New settings matrix containing additional rows
#'
#' @export
add_settings_matrix_rows <- function(settings_matrix,
                                   nrows,
                                   min_removed = NULL,
                                   max_removed = NULL,
                                   retry_limit = 10) {
    i <- 0
    num_retries <- 0
    while (i < nrows) {
        row_id <- nrow(settings_matrix) + 1
        new_row <- vector()
        # Inclusion columns
        num_inclusion_cols <- sum(startsWith(colnames(settings_matrix), "inc"))
        inclusions <- t(
            data.frame(
                random_removal(
                    num_cols = num_inclusion_cols,
                    min_removed = min_removed,
                    max_removed = max_removed
                )
            )
        )
        inclusion_names <-
            colnames(settings_matrix)[startsWith(colnames(settings_matrix), "inc")]
        colnames(inclusions) <- inclusion_names
        # Other free parameters
        snf_scheme <- sample(1:3, 1)
        eigen_or_rot <- sample(1:2, 1)
        # K and alpha range based on prior hyperparameter scans
        K <- sample(10:30, 1)
        alpha <- (sample(6:10, 1)) / 10
        # Putting it all together
        new_row <- cbind(
            row_id,
            inclusions,
            snf_scheme,
            eigen_or_rot,
            K,
            alpha)
        # Appending to settings matrix
        colnames(new_row) <- colnames(settings_matrix)
        new_row <- data.frame(new_row)
        settings_matrix <- rbind(settings_matrix, new_row)
        i <- i + 1
        # Check if newly added row already exists
        dm_no_id <- settings_matrix[, 2:length(settings_matrix)]
        num_duplicates <- length(which(
            duplicated(dm_no_id) |
            duplicated(dm_no_id, fromLast = TRUE)))
        if (num_duplicates > 0) {
            i <- i - 1
            settings_matrix <- settings_matrix[seq_len(nrow(settings_matrix)) - 1, ]
            num_retries <- num_retries + 1
        } else {
            num_retries <- 0
        }
        # Limit how many times a new row ended up already existing
        if (num_retries > retry_limit) {
            break
        }
    }
    if (num_retries > retry_limit) {
       print("Matrix row building aborted.")
       print("To keep adding rows, try raising the retry_limit parameter.")
    }
    row.names(settings_matrix) <- NULL
    return(settings_matrix)
}
