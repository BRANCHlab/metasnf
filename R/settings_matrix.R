#' Build a settings matrix
#'
#' The settings_matrix is a dataframe whose rows completely specify the
#' hyperparameters and decisions required to transform individual input
#' dataframes (found in a data_list, see ?generate_data_list) into a single
#' similarity matrix through SNF. The format of the settings matrix is as
#' follows:
#' * A column named "row_id": This column is used to keep
#'   track of the rows and should have integer values only.
#' * A column named "alpha": This column contains the value of the
#'   alpha hyperparameter that will be used on that run of the SNF pipeline.
#' * A column named "k": Like above, but for the K (nearest neighbours)
#'   hyperparameter.
#' * A column named "t": Like above, but for the t (number of iterations)
#'   hyperparameter.
#' * A column named "clust_alg": Specification of which clustering algorithm
#'   will be applied to the final similarity matrix to identify patient
#'   subtypes. By default, this column can take on the integer values 1 or 2,
#'   which correspond to spectral clustering where the number of clusters is
#'   determined by the eigen-gap or rotation cost heuristic respectively. You
#'   can learn more about this parameter here:
#'   https://branchlab.github.io/metasnf/articles/clustering_algorithms.html.
#' * A column named "cont_dist": Specification of which distance metric will be
#'   used for dataframes of purely continuous data. You can learn about this
#'   metric and its defaults here:
#'   https://branchlab.github.io/metasnf/articles/distance_metrics.html
#' * A column named "disc_dist": Like above, but for discrete dataframes.
#' * A column named "ord_dist": Like above, but for ordinal dataframes.
#' * A column named "cat_dist": Like above, but for categorical dataframes.
#' * A column named "mixed_dist": Like above, but for mixed-type (e.g.,
#'   both categorical and discrete) dataframes.
#' * One column for every input dataframe in the corresponding data_list which
#'   can either have the value of 0 or 1. The name of the column should be
#'   formatted as "inc_[]" where the square brackets are replaced with the
#'   name (as found in dl_summary(data_list)$"name") of each dataframe. When
#'   0, that dataframe will be excluded from that run of the SNF pipeline. When
#'   1, that dataframe will be included.
#'
#' @param data_list A nested list of input data from `generate_data_list()`.
#'
#' @param nrows Number of rows to generate for the settings matrix.
#'
#' @param seed (DEPRECATED) set the global seed. To ensure reproducible
#' settings matrices are generated, manually call `set.seed()` prior to
#' settings matrix generation instead of using this parameter.
#'
#' @param min_removed_inputs The smallest number of input dataframes that may be
#' randomly removed. By default, 0.
#'
#' @param max_removed_inputs The largest number of input dataframes that may be
#' randomly removed. By default, this is 1 less than all the provided input
#' dataframes in the data_list.
#'
#' @param dropout_dist Parameter controlling how the random removal of input
#' dataframes should occur. Can be "none" (no input dataframes are randomly
#' removed), "uniform" (uniformly sample between min_removed_inputs and
#' max_removed_inputs to determine number of input dataframes to remove), or
#' "exponential" (pick number of input dataframes to remove by sampling from
#'  min_removed_inputs to max_removed_inputs with an exponential distribution;
#'  the default).
#'
#' @param min_alpha The minimum value that the alpha hyperparameter can have.
#' Random assigned value of alpha for each row will be obtained by uniformly
#' sampling numbers between `min_alpha` and `max_alpha` at intervals of 0.1.
#' Cannot be used in conjunction with the `alpha_values` parameter.
#'
#' @param max_alpha The maximum value that the alpha hyperparameter can have.
#' See `min_alpha` parameter. Cannot be used in conjunction with the
#' `alpha_values` parameter.
#'
#' @param min_k The minimum value that the k hyperparameter can have.
#' Random assigned value of k for each row will be obtained by uniformly
#' sampling numbers between `min_k` and `max_k` at intervals of 1.
#' Cannot be used in conjunction with the `k_values` parameter.
#'
#' @param max_k The maximum value that the k hyperparameter can have.
#' See `min_k` parameter. Cannot be used in conjunction with the
#' `k_values` parameter.
#'
#' @param min_t The minimum value that the t hyperparameter can have.
#' Random assigned value of t for each row will be obtained by uniformly
#' sampling numbers between `min_t` and `max_t` at intervals of 1.
#' Cannot be used in conjunction with the `t_values` parameter.
#'
#' @param max_t The maximum value that the t hyperparameter can have.
#' See `min_t` parameter. Cannot be used in conjunction with the
#' `t_values` parameter.
#'
#' @param alpha_values A number or numeric vector of a set of possible values
#' that alpha can take on. Value will be obtained by uniformly sampling the
#' vector. Cannot be used in conjunction with the `min_alpha` or `max_alpha`
#' parameters.
#'
#' @param k_values A number or numeric vector of a set of possible values
#' that k can take on. Value will be obtained by uniformly sampling the
#' vector. Cannot be used in conjunction with the `min_k` or `max_k`
#' parameters.
#'
#' @param t_values A number or numeric vector of a set of possible values
#' that t can take on. Value will be obtained by uniformly sampling the
#' vector. Cannot be used in conjunction with the `min_t` or `max_t`
#' parameters.
#'
#' @param possible_snf_schemes A vector containing the possible snf_schemes to
#' uniformly randomly select from. By default, the vector contains all
#' 3 possible schemes: c(1, 2, 3). 1 corresponds to the "individual" scheme,
#' 2 corresponds to the "domain" scheme, and 3 corresponds to the "twostep"
#' scheme.
#'
#' @param clustering_algorithms A list of clustering algorithms to uniformly
#' randomly pick from when clustering. When not specified, randomly select
#' between spectral clustering using the eigen-gap heuristic and spectral
#' clustering using the rotation cost heuristic. See ?generate_clust_algs_list
#' for more details on running custom clustering algorithms.
#'
#' @param continuous_distances A vector of continuous distance metrics to use
#' when a custom distance_metrics_list is provided.
#'
#' @param discrete_distances A vector of categorical distance metrics to use
#' when a custom distance_metrics_list is provided.
#'
#' @param ordinal_distances A vector of categorical distance metrics to use
#' when a custom distance_metrics_list is provided.
#'
#' @param categorical_distances A vector of categorical distance metrics to use
#' when a custom distance_metrics_list is provided.
#'
#' @param mixed_distances A vector of mixed distance metrics to use
#' when a custom distance_metrics_list is provided.
#'
#' @param distance_metrics_list List containing distance metrics to vary over.
#' See ?generate_distance_metrics_list.
#'
#' @param snf_input_weights Nested list containing weights for when SNF is
#' used to merge individual input measures (see ?generate_snf_weights)
#'
#' @param snf_domain_weights Nested list containing weights for when SNF is
#' used to merge domains (see ?generate_snf_weights)
#'
#' @param retry_limit The maximum number of attempts to generate a novel row.
#' This function does not return matrices with identical rows. As the range of
#' requested possible settings tightens and the number of requested rows
#' increases, the risk of randomly generating a row that already exists
#' increases. If a new random row has matched an existing row `retry_limit`
#' number of times, the function will terminate.
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
                                     alpha_values = NULL,
                                     k_values = NULL,
                                     t_values = NULL,
                                     possible_snf_schemes = c(1, 2, 3),
                                     clustering_algorithms = NULL,
                                     continuous_distances = NULL,
                                     discrete_distances = NULL,
                                     ordinal_distances = NULL,
                                     categorical_distances = NULL,
                                     mixed_distances = NULL,
                                     distance_metrics_list = NULL,
                                     snf_input_weights = NULL,
                                     snf_domain_weights = NULL,
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
        alpha_values = alpha_values,
        k_values = k_values,
        t_values = t_values,
        possible_snf_schemes = possible_snf_schemes,
        clustering_algorithms = clustering_algorithms,
        continuous_distances = continuous_distances,
        discrete_distances = discrete_distances,
        ordinal_distances = ordinal_distances,
        categorical_distances = categorical_distances,
        mixed_distances = mixed_distances,
        distance_metrics_list = distance_metrics_list,
        snf_input_weights = snf_input_weights,
        snf_domain_weights = snf_domain_weights,
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
#'  Cannot be used in conjunction with the `alpha_values` parameter.
#'
#' @param max_alpha The maximum value that the alpha hyperparameter can have.
#'  See `min_alpha` parameter. Cannot be used in conjunction with the
#'  `alpha_values` parameter.
#'
#' @param min_k The minimum value that the k hyperparameter can have.
#'  Random assigned value of k for each row will be obtained by uniformly
#'  sampling numbers between `min_k` and `max_k` at intervals of 1.
#'  Cannot be used in conjunction with the `k_values` parameter.
#'
#' @param max_k The maximum value that the k hyperparameter can have.
#'  See `min_k` parameter. Cannot be used in conjunction with the
#'  `k_values` parameter.
#'
#' @param min_t The minimum value that the t hyperparameter can have.
#'  Random assigned value of t for each row will be obtained by uniformly
#'  sampling numbers between `min_t` and `max_t` at intervals of 1.
#'  Cannot be used in conjunction with the `t_values` parameter.
#'
#' @param max_t The maximum value that the t hyperparameter can have.
#'  See `min_t` parameter. Cannot be used in conjunction with the
#'  `t_values` parameter.
#'
#' @param alpha_values A number or numeric vector of a set of possible values
#'  that alpha can take on. Value will be obtained by uniformly sampling the
#'  vector. Cannot be used in conjunction with the `min_alpha` or `max_alpha`
#'  parameters.
#'
#' @param k_values A number or numeric vector of a set of possible values
#'  that k can take on. Value will be obtained by uniformly sampling the
#'  vector. Cannot be used in conjunction with the `min_k` or `max_k`
#'  parameters.
#'
#' @param t_values A number or numeric vector of a set of possible values
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
#' @param continuous_distances A vector of continuous distance metrics to use
#'  when a custom distance_metrics_list is provided.
#'
#' @param discrete_distances A vector of categorical distance metrics to use
#'  when a custom distance_metrics_list is provided.
#'
#' @param ordinal_distances A vector of categorical distance metrics to use
#'  when a custom distance_metrics_list is provided.
#'
#' @param categorical_distances A vector of categorical distance metrics to use
#'  when a custom distance_metrics_list is provided.
#'
#' @param mixed_distances A vector of mixed distance metrics to use
#'  when a custom distance_metrics_list is provided.
#'
#' @param distance_metrics_list List containing distance metrics to vary over.
#'  See ?generate_distance_metrics_list.
#'
#' @param snf_input_weights Nested list containing weights for when SNF is
#'  used to merge individual input measures (see ?generate_snf_weights)
#'
#' @param snf_domain_weights Nested list containing weights for when SNF is
#'  used to merge domains (see ?generate_snf_weights)
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
                                     alpha_values = NULL,
                                     k_values = NULL,
                                     t_values = NULL,
                                     possible_snf_schemes = c(1, 2, 3),
                                     clustering_algorithms = NULL,
                                     continuous_distances = NULL,
                                     discrete_distances = NULL,
                                     ordinal_distances = NULL,
                                     categorical_distances = NULL,
                                     mixed_distances = NULL,
                                     distance_metrics_list = NULL,
                                     snf_input_weights = NULL,
                                     snf_domain_weights = NULL,
                                     retry_limit = 10) {
    ###########################################################################
    # 1. Handling alpha hyperparameter
    ###########################################################################
    # 1a. Ensure range is specified by only one approach
    null_min_max_alpha <- is.null(min_alpha) & is.null(max_alpha)
    null_alpha_values <- is.null(alpha_values)
    if (!null_alpha_values & !null_min_max_alpha) {
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
    if (!is.null(alpha_values)) {
        if (min(alpha_values) < 0.3 | max(alpha_values) > 0.8) {
            warning(
                "Requested minimum / maximum alpha hyperparameter range is",
                " outside range empirically considere reasonable (0.3 to 0.8)."
            )
        }
    }
    # 1c. Setup alpha_values to contain values to sample from
    if (is.null(alpha_values)) {
        if (is.null(min_alpha)) {
            min_alpha <- 0.3
        }
        if (is.null(max_alpha)) {
            max_alpha <- 0.8
        }
        alpha_values <- seq(min_alpha, max_alpha, by = 0.1)
    }
    ###########################################################################
    # 2. Handling k hyperparameter
    ###########################################################################
    # 2a. Ensure range is specified by only one approach
    null_min_max_k <- is.null(min_k) & is.null(max_k)
    null_k_values <- is.null(k_values)
    if (!null_k_values & !null_min_max_k) {
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
    if (!is.null(k_values)) {
        if (min(k_values) < 10 | max(k_values) > 100) {
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
    # 2c. Setup k_values to contain values to sample from
    if (is.null(k_values)) {
        if (is.null(min_k)) {
            min_k <- 10
        }
        if (is.null(max_k)) {
            max_k <- 99
        }
        k_values <- seq(min_k, max_k, by = 1)
    }
    ###########################################################################
    # 3. Handling t hyperparameter
    ###########################################################################
    # 3a. Ensure range is specified by only one approach
    null_min_max_t <- is.null(min_t) & is.null(max_t)
    null_t_values <- is.null(t_values)
    if (!null_t_values & !null_min_max_t) {
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
    if (!is.null(t_values)) {
        if (min(t_values) < 10 | max(t_values) > 20) {
            warning(
                "The original SNF paper recommends a t between 10 to 20.",
                " Empirically, setting t above 20 is always sufficient for",
                " SNF to converge. This warning is raised anytime a user",
                " tries to set a t value smaller than 10 or larger than 20."
            )
        }
    }
    # 3c. Setup t_values to contain values to sample from
    if (is.null(t_values)) {
        if (is.null(min_t)) {
            min_t <- 20
        }
        if (is.null(max_t)) {
            max_t <- 20
        }
        t_values <- seq(min_t, max_t, by = 1)
    }
    ###########################################################################
    # 4. Handling distance metrics
    ###########################################################################
    if (is.null(distance_metrics_list)) {
        distance_metrics_list <- generate_distance_metrics_list()
    }
    ###########################################################################
    # 5. Set the random seed (if provided)
    ###########################################################################
    if (!is.null(seed)) {
        stop(
            "Adjusting the global seed through this function is deprecated.",
            " Please call `set.seed()` manually instead."
        )
    }
    ###########################################################################
    # 6. Begin the loop that will generate new random settings_matrix rows
    ###########################################################################
    i <- 0
    num_retries <- 0
    while (i < nrows) {
        row_id <- nrow(settings_matrix) + 1
        new_row <- vector()
        # Inclusion columns
        inclusions <- random_removal(
            columns = colnames(settings_matrix),
            min_removed_inputs = min_removed_inputs,
            max_removed_inputs = max_removed_inputs,
            dropout_dist = dropout_dist
        )
        #######################################################################
        # 7. Pick random values uniformly
        #######################################################################
        # The behaviour of sample is different when it receives 1 number vs.
        #  a vector of numbers. Rather than just picking that 1 number, it will
        #  pick a random number from 1 to that number. If the user's range
        #  is only a single value, this syntax will avoid sampling from 1 to
        #  that value.
        snf_scheme <- sample(possible_snf_schemes, 1)
        clust_alg <- sample(1:2, 1)
        alpha <- alpha_values[sample.int(length(alpha_values), 1)]
        k <- k_values[sample.int(length(k_values), 1)]
        t <- t_values[sample.int(length(t_values), 1)]
        if (is.null(clustering_algorithms)) {
            # there are currently 2 defaults (spectral_eig/rot) to choose from
            clust_alg <- sample(1:2, 1)
        } else {
            clust_alg <- sample(1:length(clustering_algorithms), 1)
        }
        #######################################################################
        # 8. Distance metrics
        #######################################################################
        if (is.null(continuous_distances)) {
            cont_dist <- sample(
                1:length(distance_metrics_list$"continuous_distances"),
                1
            )
        } else {
            cont_dist <- resample(continuous_distances, 1)
        }
        if (is.null(discrete_distances)) {
            disc_dist <- sample(
                1:length(distance_metrics_list$"discrete_distances"),
                1
            )
        } else {
            disc_dist <- resample(discrete_distances, 1)
        }
        if (is.null(ordinal_distances)) {
            ord_dist <- sample(
                1:length(distance_metrics_list$"ordinal_distances"),
                1
            )
        } else {
            ord_dist <- resample(ordinal_distances, 1)
        }
        if (is.null(categorical_distances)) {
            cat_dist <- sample(
                1:length(distance_metrics_list$"categorical_distances"),
                1
            )
        } else {
            cat_dist <- resample(categorical_distances, 1)
        }
        if (is.null(mixed_distances)) {
            mix_dist <- sample(
                1:length(distance_metrics_list$"mixed_distances"),
                1
            )
        } else {
            mix_dist <- resample(mixed_distances, 1)
        }
        #######################################################################
        # 7. Combine selected values to a single dataframe row
        #######################################################################
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
        #######################################################################
        # 8. Append the new row to the full settings_matrix
        #######################################################################
        colnames(new_row) <- colnames(settings_matrix)
        new_row <- data.frame(new_row)
        settings_matrix <- rbind(settings_matrix, new_row)
        i <- i + 1
        #######################################################################
        # 9. Check if newly added row already exists in settings_matrix
        #######################################################################
        dm_no_id <- settings_matrix[, 2:length(settings_matrix)]
        num_duplicates <- length(which(
            duplicated(dm_no_id) |
            duplicated(dm_no_id, fromLast = TRUE)))
        if (num_duplicates > 0) {
            i <- i - 1
            settings_matrix <-
                settings_matrix[seq_len(nrow(settings_matrix)) - 1, ]
            num_retries <- num_retries + 1
        } else {
            num_retries <- 0
        }
        # Limit how many times a new row ended up already existing
        if (num_retries > retry_limit) {
           stop(
                "Matrix building failed to converge. To keep adding rows, try",
                " raising the retry_limit parameter or specifying a larger",
                " range of tunable parameters."
            )
        }
    }
    row.names(settings_matrix) <- NULL
    return(settings_matrix)
}

#' Generate random removal sequence
#'
#' Helper function to contribute to rows within the settings matrix. Number of
#'  columns removed follows a uniform or exponential probability distribution.
#'
#' @param columns Columns of the settings_matrix that are passed in
#'
#' @param min_removed_inputs The smallest number of input dataframes that may
#'  be randomly removed.
#'
#' @param max_removed_inputs The largest number of input dataframes that may be
#'  randomly removed.
#'
#' @param dropout_dist Indication of how input dataframes should be dropped.
#'  can be "none" (no dropout), "uniform" (uniformly draw number between min
#'  and max removed inputs), or "exponential" (like uniform, but using an
#'  exponential distribution; default).
#'
#' @return inclusions_df Dataframe that can be rbind'ed to the settings_matrix
#'
#' @export
random_removal <- function(columns,
                           min_removed_inputs,
                           max_removed_inputs,
                           dropout_dist = "exponential") {
    ###########################################################################
    # 1. Define features used by all dropout_dist values
    ###########################################################################
    # vector containing names of the input dataframes that may be dropped
    inclusion_columns <- columns[startsWith(columns, "inc")]
    # number of droppable input dataframes
    num_cols <- length(inclusion_columns)
    ###########################################################################
    # 2. "none" (no) random input dataframe dropout
    ###########################################################################
    # If the user requests no random dropout, just return a dataframe row that
    #  has 1 (include) for every input dataframe
    if (dropout_dist == "none") {
        inclusions_df <- rep(1, num_cols) |>
            t() |>
            data.frame()
        colnames(inclusions_df) <- inclusion_columns
        rownames(inclusions_df) <- NULL
        return(inclusions_df)
    }
    ###########################################################################
    # 3. Otherwise, determine min and max number of inputs to remove
    ###########################################################################
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
    ###########################################################################
    # 4. "uniform" - pick a uniformly random number of inputs to remove
    ###########################################################################
    if (dropout_dist == "uniform") {
        possible_number_removed <- seq(
            min_removed_inputs,
            max_removed_inputs,
            by = 1
        )
        num_removed <- resample(possible_number_removed, 1)
    }
    ###########################################################################
    # 5. "exponential" - pick an exponentially distributed number of inputs
    #     to remove
    ###########################################################################
    if (dropout_dist == "exponential") {
        # 10000 randomly distributed values
        rand_vals <- stats::rexp(10000)
        # Scale the values to have a maximum of 1. Because there are so many
        #  exponentially distributed numbers, the min value will be quite
        #  close to 0.
        rand_vals <- rand_vals / max(rand_vals)
        # Difference indicates how many possible inputs may be dropped
        difference <- max_removed_inputs - min_removed_inputs
        #  E.g. if we are dropping between 5 and 20 input dataframes, this will
        #  ensure the largest value is 15. Because of the large amount of
        #  numbers, the smallest value will still be quite close to 0.
        rand_vals <- rand_vals * difference
        # After this addition, we can expect the smallest value to be close to
        #  the minimum number of removed inputs (e.g, 5) and the biggest value
        #  to be quite close to the maximum number of removed inputs (e.g., 20)
        rand_vals <- rand_vals + min_removed_inputs
        # From here, simply round the pool of numbers to make them all ints and
        #  select one uniformly at random.
        rand_vals <- round(rand_vals)
        num_removed <- sample(rand_vals, 1)
        # There very likely could be a much simpler way to achieve this goal.
    }
    ###########################################################################
    # 6. Randomly remove the calculated number of input dataframes to remove
    ###########################################################################
    # Vector of 0s the size of the number of inputs to remove
    remove_placeholders <- rep(0, num_removed)
    # Vector of 1s the size of the number of inputs to keep
    keep_placeholders <- rep(1, num_cols - num_removed)
    # Concatenate the two and shuffle them
    unshuffled_removals <- c(remove_placeholders, keep_placeholders)
    shuffled_removals <- sample(unshuffled_removals)
    # Turn that shuffled vector into a dataframe row and return that row to be
    #  merged into the rest of the settings_matrix
    inclusions_df <- shuffled_removals |>
        data.frame() |>
        t()
    colnames(inclusions_df) <- inclusion_columns
    rownames(inclusions_df) <- NULL
    return(inclusions_df)
}
