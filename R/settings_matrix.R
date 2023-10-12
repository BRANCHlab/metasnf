#' Build a settings matrix
#'
#' @param data_list A data list object to determine variables for inclusion and
#'  exclusion.
#'
#' @param nrows Number of rows to generate for the settings matrix.
#'
#' @param seed set a seed for the random matrix generation. Setting this value
#'  will change the seed of the global environment.
#'
#' @param min_removed_inputs The smallest number of input dataframes that may be
#'  randomly removed. By default, 0.
#'
#' @param max_removed_inputs The largest number of input dataframes that may be
#'  randomly removed. By default, this is 1 less than all the provided input
#'  dataframes in the data_list.
#'
#' @param dropout_dist Parameter controlling how the random removal of input
#'  dataframes should occur. Can be "none" (no input dataframes are randomly
#'  removed), "uniform" (uniformly sample between min_removed_inputs and max_removed_inputs
#'  to determine number of input dataframes to remove), or "exponential" (pick
#'  number of input dataframes to remove by sampling from min_removed_inputs to
#'  max_removed_inputs with an exponential distribution; default).
#'
#' @param min_alpha The minimum value that the alpha hyperparameter can have.
#'  Random assigned value of alpha for each row will be obtained by uniformly
#'  sampling numbers between `min_alpha` and `max_alpha` at intervals of 0.1.
#'  Cannot be used in conjunction with the `possible_alpha` parameter.
#'
#' @param max_alpha The maximum value that the alpha hyperparameter can have.
#'  See `min_alpha` parameter. Cannot be used in conjunction with the
#'  `possible_alpha` parameter.
#'
#' @param min_k The minimum value that the k hyperparameter can have.
#'  Random assigned value of k for each row will be obtained by uniformly
#'  sampling numbers between `min_k` and `max_k` at intervals of 1.
#'  Cannot be used in conjunction with the `possible_k` parameter.
#'
#' @param max_k The maximum value that the k hyperparameter can have.
#'  See `min_k` parameter. Cannot be used in conjunction with the
#'  `possible_k` parameter.
#'
#' @param min_t The minimum value that the t hyperparameter can have.
#'  Random assigned value of t for each row will be obtained by uniformly
#'  sampling numbers between `min_t` and `max_t` at intervals of 1.
#'  Cannot be used in conjunction with the `possible_t` parameter.
#'
#' @param max_t The maximum value that the t hyperparameter can have.
#'  See `min_t` parameter. Cannot be used in conjunction with the
#'  `possible_t` parameter.
#'
#' @param possible_alpha A number or numeric vector of a set of possible values
#'  that alpha can take on. Value will be obtained by uniformly sampling the
#'  vector. Cannot be used in conjunction with the `min_alpha` or `max_alpha`
#'  parameters.
#'
#' @param possible_k A number or numeric vector of a set of possible values
#'  that k can take on. Value will be obtained by uniformly sampling the
#'  vector. Cannot be used in conjunction with the `min_k` or `max_k`
#'  parameters.
#'
#' @param possible_t A number or numeric vector of a set of possible values
#'  that t can take on. Value will be obtained by uniformly sampling the
#'  vector. Cannot be used in conjunction with the `min_t` or `max_t`
#'  parameters.
#'
#' @param possible_snf_schemes A vector containing the possible snf_schemes to
#'  uniformly randomly select from. By default, the vector contains all
#'  3 possible schemes: c(1, 2, 3). 1 corresponds to the "individual" scheme,
#'  2 corresponds to the "domain" scheme, and 3 corresponds to the "twostep"
#'  scheme.
#'
#' @param clustering_algorithms A list of clustering algorithms to uniformly
#'  randomly pick from when clustering. When not specified, randomly select
#'  between spectral clustering using the eigen-gap heuristic and spectral
#'  clustering using the rotation cost heuristic. See ?generate_clust_algs_list
#'  for more details on running custom clustering algorithms.
#'
#' @param continuous_distances A list of distance metrics to use
#'  anytime raw data is converted to a distance matrix as an intermediate step.
#'  By default, this will standardized normalized Euclidean. See
#'  ?generate_distance_list for more details on using custom distance metrics.
#'
#' @param discrete_distances Like `continuous_distances`, but
#'  for discrete data. By default, uses standardized normalized Euclidean.
#'
#' @param ordinal_distances Like `continuous_distances`, but
#'  for ordinal data. By default, uses standardized normalized Euclidean.
#'
#' @param categorical_distances Like `continuous_distances`, but
#'  for categorical data. By default, uses Gower's distance.
#'
#' @param mixed_distances Like `continuous_distances`, but
#'  for mixed data. By default, uses gower distance.
#'
#' @param retry_limit The maximum number of attempts to generate a novel row.
#'  This function does not return matrices with identical rows. As the range of
#'  requested possible settings tightens and the number of requested rows
#'  increases, the risk of randomly generating a row that already exists
#'  increases. If a new random row has matched an existing row `retry_limit`
#'  number of times, the function will terminate.
#'
#' @return settings_matrix A settings matrix
#'
#' @export
generate_settings_matrix <- function(data_list,
                                     seed = NULL,
                                     nrows = 0,
                                     min_removed_inputs = 0,
                                     max_removed_inputs = length(
                                         data_list
                                     ) - 1,
                                     dropout_dist = "exponential",
                                     min_alpha = NULL,
                                     max_alpha = NULL,
                                     min_k = NULL,
                                     max_k = NULL,
                                     min_t = NULL,
                                     max_t = NULL,
                                     possible_alpha = NULL,
                                     possible_k = NULL,
                                     possible_t = NULL,
                                     possible_snf_schemes = c(1, 2, 3),
                                     clustering_algorithms = NULL,
                                     continuous_distances = NULL,
                                     discrete_distances = NULL,
                                     ordinal_distances = NULL,
                                     categorical_distances = NULL,
                                     mixed_distances = NULL,
                                     retry_limit = 10) {
    settings_matrix_columns <- c(
        "row_id",
        "alpha",
        "k",
        "t",
        "snf_scheme",
        "clust_alg",
        "cont_dist",
        "disc_dist",
        "ord_dist",
        "cat_dist",
        "mix_dist",
        paste0("inc_", summarize_dl(data_list)$"name")
    )
    settings_matrix_base <- as.data.frame(
        matrix(
            0,
            ncol = length(settings_matrix_columns),
            nrow = 0
        )
    )
    colnames(settings_matrix_base) <- settings_matrix_columns
    settings_matrix <- add_settings_matrix_rows(
        settings_matrix = settings_matrix_base,
        seed = seed,
        nrows = nrows,
        min_removed_inputs = min_removed_inputs,
        max_removed_inputs = max_removed_inputs,
        dropout_dist = dropout_dist,
        min_alpha = min_alpha,
        max_alpha = max_alpha,
        min_k = min_k,
        max_k = max_k,
        min_t = min_t,
        max_t = max_t,
        possible_alpha = possible_alpha,
        possible_k = possible_k,
        possible_t = possible_t,
        possible_snf_schemes = possible_snf_schemes,
        clustering_algorithms = clustering_algorithms,
        continuous_distances = continuous_distances,
        discrete_distances = discrete_distances,
        ordinal_distances = ordinal_distances,
        categorical_distances = categorical_distances,
        mixed_distances = mixed_distances,
        retry_limit = retry_limit
    )
    return(settings_matrix)
}

#' Add settings matrix rows
#'
#' @param settings_matrix The existing settings matrix
#'
#' @param nrows Number of rows to generate for the settings matrix.
#'
#' @param seed set a seed for the random matrix generation. Setting this value
#'  will change the seed of the global environment.
#'
#' @param min_removed_inputs The smallest number of input dataframes that may be
#'  randomly removed. By default, 0.
#'
#' @param max_removed_inputs The largest number of input dataframes that may be
#'  randomly removed. By default, this is 1 less than all the provided input
#'  dataframes in the data_list.
#'
#' @param dropout_dist Parameter controlling how the random removal of input
#'  dataframes should occur. Can be "none" (no input dataframes are randomly
#'  removed), "uniform" (uniformly sample between min_removed_inputs and max_removed_inputs
#'  to determine number of input dataframes to remove), or "exponential" (pick
#'  number of input dataframes to remove by sampling from min_removed_inputs to
#'  max_removed_inputs with an exponential distribution; default).
#'
#' @param min_alpha The minimum value that the alpha hyperparameter can have.
#'  Random assigned value of alpha for each row will be obtained by uniformly
#'  sampling numbers between `min_alpha` and `max_alpha` at intervals of 0.1.
#'  Cannot be used in conjunction with the `possible_alpha` parameter.
#'
#' @param max_alpha The maximum value that the alpha hyperparameter can have.
#'  See `min_alpha` parameter. Cannot be used in conjunction with the
#'  `possible_alpha` parameter.
#'
#' @param min_k The minimum value that the k hyperparameter can have.
#'  Random assigned value of k for each row will be obtained by uniformly
#'  sampling numbers between `min_k` and `max_k` at intervals of 1.
#'  Cannot be used in conjunction with the `possible_k` parameter.
#'
#' @param max_k The maximum value that the k hyperparameter can have.
#'  See `min_k` parameter. Cannot be used in conjunction with the
#'  `possible_k` parameter.
#'
#' @param min_t The minimum value that the t hyperparameter can have.
#'  Random assigned value of t for each row will be obtained by uniformly
#'  sampling numbers between `min_t` and `max_t` at intervals of 1.
#'  Cannot be used in conjunction with the `possible_t` parameter.
#'
#' @param max_t The maximum value that the t hyperparameter can have.
#'  See `min_t` parameter. Cannot be used in conjunction with the
#'  `possible_t` parameter.
#'
#' @param possible_alpha A number or numeric vector of a set of possible values
#'  that alpha can take on. Value will be obtained by uniformly sampling the
#'  vector. Cannot be used in conjunction with the `min_alpha` or `max_alpha`
#'  parameters.
#'
#' @param possible_k A number or numeric vector of a set of possible values
#'  that k can take on. Value will be obtained by uniformly sampling the
#'  vector. Cannot be used in conjunction with the `min_k` or `max_k`
#'  parameters.
#'
#' @param possible_t A number or numeric vector of a set of possible values
#'  that t can take on. Value will be obtained by uniformly sampling the
#'  vector. Cannot be used in conjunction with the `min_t` or `max_t`
#'  parameters.
#'
#' @param possible_snf_schemes A vector containing the possible snf_schemes to
#'  uniformly randomly select from. By default, the vector contains all
#'  3 possible schemes: c(1, 2, 3). 1 corresponds to the "individual" scheme,
#'  2 corresponds to the "domain" scheme, and 3 corresponds to the "twostep"
#'  scheme.
#'
#' @param clustering_algorithms A list of clustering algorithms to uniformly
#'  randomly pick from when clustering. When not specified, randomly select
#'  between spectral clustering using the eigen-gap heuristic and spectral
#'  clustering using the rotation cost heuristic. See ?generate_clust_algs_list
#'  for more details on running custom clustering algorithms.
#'
#' @param continuous_distances A list of distance metrics to use
#'  anytime raw data is converted to a distance matrix as an intermediate step.
#'  By default, this will standardized normalized Euclidean. See
#'  ?generate_distance_list for more details on using custom distance metrics.
#'
#' @param discrete_distances Like `continuous_distances`, but
#'  for discrete data. By default, uses standardized normalized Euclidean.
#'
#' @param ordinal_distances Like `continuous_distances`, but
#'  for ordinal data. By default, uses standardized normalized Euclidean.
#'
#' @param categorical_distances Like `continuous_distances`, but
#'  for categorical data. By default, uses Gower's distance.
#'
#' @param mixed_distances Like `continuous_distances`, but
#'  for mixed data. By default, uses gower distance.
#'
#' @param retry_limit The maximum number of attempts to generate a novel row.
#'  This function does not return matrices with identical rows. As the range of
#'  requested possible settings tightens and the number of requested rows
#'  increases, the risk of randomly generating a row that already exists
#'  increases. If a new random row has matched an existing row `retry_limit`
#'  number of times, the function will terminate.
#'
#' @return settings_matrix A settings matrix
#'
#' @export
add_settings_matrix_rows <- function(settings_matrix,
                                     seed = NULL,
                                     nrows = 0,
                                     min_removed_inputs = 0,
                                     max_removed_inputs = sum(
                                         startsWith(
                                             colnames(settings_matrix),
                                             "inc_"
                                         )
                                     ) - 1,
                                     dropout_dist = "exponential",
                                     min_alpha = NULL,
                                     max_alpha = NULL,
                                     min_k = NULL,
                                     max_k = NULL,
                                     min_t = NULL,
                                     max_t = NULL,
                                     possible_alpha = NULL,
                                     possible_k = NULL,
                                     possible_t = NULL,
                                     possible_snf_schemes = c(1, 2, 3),
                                     clustering_algorithms = NULL,
                                     continuous_distances = NULL,
                                     discrete_distances = NULL,
                                     ordinal_distances = NULL,
                                     categorical_distances = NULL,
                                     mixed_distances = NULL,
                                     retry_limit = 10) {
    ###########################################################################
    # 1. Handling alpha hyperparameter
    ###########################################################################
    # 1a. Ensure range is specified by only one approach
    null_min_max_alpha <- is.null(min_alpha) & is.null(max_alpha)
    null_possible_alpha <- is.null(possible_alpha)
    if (!null_possible_alpha & !null_min_max_alpha) {
        stop(
            paste0(
                "alpha parameter can be controlled using either the min/max",
                " parameters or using the possible parameter - not both."
            )
        )
    }
    # 1b. Ensure specified upper and lower bounds are sensible
    if (!is.null(min_alpha)) {
        if (min_alpha < 0.3) {
            warning(
                "Requested minimum / maximum alpha hyperparameter range is",
                " outside range empirically considere reasonable (0.3 to 0.8)."
            )
        }
    }
    if (!is.null(max_alpha)) {
        if (max_alpha > 0.8) {
            warning(
                "Requested minimum / maximum alpha hyperparameter range is",
                " outside range empirically considere reasonable (0.3 to 0.8)."
            )
        }
    }
    if (!is.null(possible_alpha)) {
        if (min(possible_alpha) < 0.3 | max(possible_alpha) > 0.8) {
            warning(
                "Requested minimum / maximum alpha hyperparameter range is",
                " outside range empirically considere reasonable (0.3 to 0.8)."
            )
        }
    }
    # 1c. Setup possible_alpha to contain values to sample from
    if (is.null(possible_alpha)) {
        if (is.null(min_alpha)) {
            min_alpha <- 0.3
        }
        if (is.null(max_alpha)) {
            max_alpha <- 0.8
        }
        possible_alpha <- seq(min_alpha, max_alpha, by = 0.1)
    }
    ###########################################################################
    # 2. Handling k hyperparameter
    ###########################################################################
    # 2a. Ensure range is specified by only one approach
    null_min_max_k <- is.null(min_k) & is.null(max_k)
    null_possible_k <- is.null(possible_k)
    if (!null_possible_k & !null_min_max_k) {
        stop(
            paste0(
                "k parameter can be controlled using either the min/max",
                " parameters or using the possible parameter - not both."
            )
        )
    }
    # 2b. Ensure specified upper and lower bounds are sensible
    if (!is.null(min_k)) {
        if (min_k < 10) {
            warning(
                "The original SNF paper recommends setting k to either the",
                " number of patients divided by the expected number of",
                " clusters or the number of patients divided by 10 when the",
                " expected number of clusters was unknown. This warning is",
                " raised anytime a user tries to set a k value smaller than",
                " 10 or larger than 100."
            )
        }
    }
    if (!is.null(max_k)) {
        if (max_k > 100) {
            warning(
                "The original SNF paper recommends setting k to either the",
                " number of patients divided by the expected number of",
                " clusters or the number of patients divided by 10 when the",
                " expected number of clusters was unknown. This warning is",
                " raised anytime a user tries to set a k value smaller than",
                " 10 or larger than 100."
            )
        }
    }
    if (!is.null(possible_k)) {
        if (min(possible_k) < 10 | max(possible_k) > 100) {
            warning(
                "The original SNF paper recommends setting k to either the",
                " number of patients divided by the expected number of",
                " clusters or the number of patients divided by 10 when the",
                " expected number of clusters was unknown. This warning is",
                " raised anytime a user tries to set a k value smaller than",
                " 10 or larger than 100."
            )
        }
    }
    # 2c. Setup possible_k to contain values to sample from
    if (is.null(possible_k)) {
        if (is.null(min_k)) {
            min_k <- 10
        }
        if (is.null(max_k)) {
            max_k <- 100
        }
        possible_k <- seq(min_k, max_k, by = 1)
    }
    ###########################################################################
    # 3. Handling t hyperparameter
    ###########################################################################
    # 3a. Ensure range is specified by only one approach
    null_min_max_t <- is.null(min_t) & is.null(max_t)
    null_possible_t <- is.null(possible_t)
    if (!null_possible_t & !null_min_max_t) {
        stop(
            paste0(
                "t parameter can be controlled using either the min/max",
                " parameters or using the possible parameter - not both."
            )
        )
    }
    # 3b. Ensure specified upper and lower bounds are sensible
    if (!is.null(min_t)) {
        if (min_t < 10) {
            warning(
                "The original SNF paper recommends a t between 10 and 20.",
                " Empirically, setting t above 20 is always sufficient for",
                " SNF to converge. This warning is raised anytime a user",
                " tries to set a t value smaller than 10 or larger than 20."
            )
        }
    }
    if (!is.null(max_t)) {
        if (max_t > 20) {
            warning(
                "The original SNF paper recommends a t between 10 to 20.",
                " Empirically, setting t above 20 is always sufficient for",
                " SNF to converge. This warning is raised anytime a user",
                " tries to set a t value smaller than 10 or larger than 20."
            )
        }
    }
    if (!is.null(possible_t)) {
        if (min(possible_t) < 10 | max(possible_t) > 20) {
            warning(
                "The original SNF paper recommends a t between 10 to 20.",
                " Empirically, setting t above 20 is always sufficient for",
                " SNF to converge. This warning is raised anytime a user",
                " tries to set a t value smaller than 10 or larger than 20."
            )
        }
    }
    # 3c. Setup possible_t to contain values to sample from
    if (is.null(possible_t)) {
        if (is.null(min_t)) {
            min_t <- 20
        }
        if (is.null(max_t)) {
            max_t <- 20
        }
        possible_t <- seq(min_t, max_t, by = 1)
    }
    ###########################################################################
    # X. Set the random seed (if provided)
    ###########################################################################
    if (!is.null(seed)) {
        set.seed(seed)
        print("The global seed has been changed!")
    }
    ###########################################################################
    # X. Begin the loop that will generate new random settings_matrix rows
    ###########################################################################
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
                    min_removed_inputs = min_removed_inputs,
                    max_removed_inputs = max_removed_inputs
                )
            )
        )
        inclusion_names <-
            colnames(settings_matrix)[startsWith(colnames(settings_matrix), "inc")]
        colnames(inclusions) <- inclusion_names
        # Other free parameters
        snf_scheme <- sample(possible_snf_schemes, 1)
        clust_alg <- sample(1:2, 1)
        #######################################################################
        # X. Pick random values uniformly
        #######################################################################
        # The behaviour of sample is different when it receives 1 number vs.
        #  a vector of numbers. Rather than just picking that 1 number, it will
        #  pick a random number from 1 to that number. If the user's range
        #  is only a single value, this syntax will avoid sampling from 1 to
        #  that value.
        alpha <- possible_alpha[sample.int(length(possible_alpha), 1)]
        k <- possible_k[sample.int(length(possible_k), 1)]
        t <- possible_t[sample.int(length(possible_t), 1)]
        cont_dist <- 1
        disc_dist <- 1
        ord_dist <- 1
        cat_dist <- 1
        mix_dist <- 1
        # Putting it all together
        new_row <- cbind(
            row_id,
            alpha,
            k,
            t,
            snf_scheme,
            clust_alg,
            cont_dist,
            disc_dist,
            ord_dist,
            cat_dist,
            mix_dist,
            inclusions
        )
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

#' Generate random removal sequence
#'
#' Helper function to contribute to rows within the settings matrix. Number of
#'  columns removed follows the exponential probability distribution to
#'  typically keep all or most columns.
#'
#' @param num_cols Number of feature elements in consideration for exclusion
#'
#' @param min_removed_inputs The smallest number of input dataframes that may
#'  be randomly removed.
#'
#' @param max_removed_inputs The largest number of input dataframes that may be
#'  randomly removed.
#'
#' @return shuffled_removals Binary vector sequence indicating if a column
#'  should be included (1) or excluded (0)
#'
#' @export
random_removal <- function(num_cols,
                           min_removed_inputs,
                           max_removed_inputs) {
    # Generate 10,000 random numbers according to exponential distribution
    if (is.null(min_removed_inputs)) {
        min_removed_inputs <- 0
    }
    if (is.null(max_removed_inputs)) {
        max_removed_inputs <- num_cols - 1
    }
    if (max_removed_inputs >= num_cols || min_removed_inputs < 0) {
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
    difference <- max_removed_inputs - min_removed_inputs
    # Scale the values to range from 0 to 1
    rand_vals <- rand_vals / max(rand_vals)
    # Multiply by the difference
    rand_vals <- rand_vals * difference
    rand_vals <- rand_vals + min_removed_inputs
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

