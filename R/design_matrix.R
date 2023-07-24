#' Build a design matrix - but softcoded
#'
#' @param data_list a data list object to determine variables for inclusion/exclusion
#' @param nrows number of design matrix rows
#' @param seed set a seed for the random matrix generation. Note that this
#' @param retry_limit The maximum number of attempts to generate a novel row
#'  affects the global seed.
#'
#' @return design_matrix A design matrix
#'
#' @export
generate_design_matrix <- function(data_list,
                                   nrows = 0,
                                   seed = NULL,
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
    design_matrix_base <- as.data.frame(
        matrix(
            0,
            ncol = length(dm_cols),
            nrow = 0
        )
    )
    colnames(design_matrix_base) <- dm_cols
    design_matrix <- add_design_matrix_rows(design_matrix_base, nrows)
    return(design_matrix)
}

#' Generate random removal sequence
#'
#' Helper function to contribute to rows within the design matrix.
#' Number of columns removed follows the exponential probability distribution
#' to typically keep all or most columns.
#'
#' @param num_cols Number of feature columns in consideration for exclusion
#'
#' @return shuffled_removals Binary vector sequence indicating if a column
#' should be included (1) or excluded (0)
#'
#' @export
random_removal <- function(num_cols) {
    rand_vals <- stats::rexp(10000)
    rand_vals <- rand_vals / max(rand_vals) * num_cols
    rand_vals <- floor(rand_vals)
    num_removed <- sample(rand_vals, 1)
    remove_placeholders <- rep(0, num_removed)
    keep_placeholders <- rep(1, num_cols - num_removed)
    unshuffled_removals <- c(remove_placeholders, keep_placeholders)
    shuffled_removals <- sample(unshuffled_removals)
    return(shuffled_removals)
}

#' Add design matrix rows
#'
#' @param design_matrix The existing design matrix
#' @param nrows The number of rows to be added to the design matrix
#' @param retry_limit The maximum number of attempts to generate a novel row
#'
#' @return design_matrix New design matrix containing additional rows
#'
#' @export
add_design_matrix_rows <- function(design_matrix, nrows, retry_limit = 10) {
    i <- 0
    num_retries <- 0
    while (i < nrows) {
        row_id <- nrow(design_matrix) + 1
        new_row <- vector()
        # Inclusion columns
        num_inclusion_cols <- sum(startsWith(colnames(design_matrix), "inc"))
        inclusions <- t(data.frame(random_removal(num_inclusion_cols)))
        inclusion_names <-
            colnames(design_matrix)[startsWith(colnames(design_matrix), "inc")]
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
        # Appending to design matrix
        colnames(new_row) <- colnames(design_matrix)
        new_row <- data.frame(new_row)
        design_matrix <- rbind(design_matrix, new_row)
        i <- i + 1
        # Check if newly added row already exists
        dm_no_id <- design_matrix[, 2:length(design_matrix)]
        num_duplicates <- length(which(
            duplicated(dm_no_id) |
            duplicated(dm_no_id, fromLast = TRUE)))
        if (num_duplicates > 0) {
            i <- i - 1
            design_matrix <- design_matrix[seq_len(nrow(design_matrix)) - 1, ]
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
    row.names(design_matrix) <- NULL
    return(design_matrix)
}

#' Build design matrix for scanning alpha and K
#'
#' @return design_matrix Adds the standard grid expansion for SNF hyperparams
#'
#' @export
generate_design_matrix_ak <- function() {
    design_matrix <- generate_design_matrix()
    design_matrix[1:80, ] <- 1
    hyperparam_grid <- expand.grid(1:10, 3:10)
    colnames(hyperparam_grid) <- c("K", "alpha")
    design_matrix$K <- hyperparam_grid$K * 10
    design_matrix$alpha <- hyperparam_grid$alpha / 10
    return(design_matrix)
}
