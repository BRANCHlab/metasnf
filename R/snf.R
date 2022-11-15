#' Calculate distance matrices
#'
#' @description
#' Given a dataframe of numerical variables, return a euclidean distance matrix
#'
#' @param df Raw dataframe with subject IDs in column 1
#' @param input_type Either "numeric" (resulting in euclidean distances),
#'  "categorical" (resulting in binary distances), or "mixed" (resulting in
#'  gower distances)
#'
#' @return dist_matrix Matrix of inter-observation distances
#'
#' @export
get_dist_matrix <- function(df, input_type) {
    # Move subject keys into dataframe rownames
    df <- data.frame(df, row.names = 1)
    if (input_type == "numeric") {
        dist_matrix <- as.matrix(stats::dist(df, method = "euclidean"))
    } else if (input_type == "categorical") {
        dist_matrix <- as.matrix(stats::dist(df, method = "binary"))
    } else if (input_type == "mixed") {
        dist_matrix <- as.matrix(cluster::daisy(df, metric = "gower"))
    } else {
        rlang::abort(
            paste0("The value ", input_type, " is not a valid input type."),
            class = "invalid_input")
    }
    return(dist_matrix)
}

# Given clustered subjects and outcome measures, evaluate clustering utility
evaluate_clustering <- function() {
    # Did eigenvalue and rot matrix give same number of clusters?
    return(NULL)
}

#' Build a design matrix skeleton
#'
#' @description
#' Construct the base of the design matrix, a dataframe that initially contains
#'  rows that each provide information about the hyperparamters with which
#'  patient subtyping should occur for a single iteration. After executing the
#'  subtyping specified by a given row, that row is extended with detailed
#'  information about the resulting subtype membership of each included subject
#'  and the efficacy of the subtyping solution in distinguishing subjects by
#'  PPCS.
#'
#' @return design_matrix A skeleton dataframe to build a design matrix out of
#'
#' @export
build_design_matrix_base <- function() {
    design_matrix <- data.frame(
        row_id = numeric(),
        inc_mtbi_loc = numeric(),
        inc_mtbi_mechanism = numeric(),
        inc_mtbi_mem_daze = numeric(),
        inc_income = numeric(),
        inc_interview_age = numeric(),
        inc_mtbi_age = numeric(),
        inc_pubertal_status = numeric(),
        inc_race_cd = numeric(),
        inc_race_ed = numeric(),
        inc_sex = numeric(),
        inc_wmndf = numeric(),
        inc_headaches = numeric(),
        inc_mtbi_count = numeric(),
        inc_gord_cor = numeric(),
        inc_gord_var = numeric(),
        inc_subc_cor = numeric(),
        inc_subc_var = numeric(),
        inc_cort_sa = numeric(),
        inc_cort_t = numeric(),
        inc_subc_v = numeric(),
        snf_scheme = numeric(),
        neuroimaging_domain = numeric(),
        variable_weighting = numeric(),
        pca = numeric(),
        output_vars = numeric(),
        eigen_or_rot = numeric(),
        stringsAsFactors = FALSE)
    row.names(design_matrix) <- NULL
    return(design_matrix)
}

#' Build a design matrix skeleton FOR TESTING - It's just smaller
#'
#' @description
#' Construct the base of the design matrix for small testing purposes
#'
#' @return design_matrix A skeleton dataframe to build a design matrix out of
#'
#' @export
build_design_matrix_base_t <- function() {
    design_matrix <- data.frame(
        row_id = numeric(),
        inc_mtbi_loc = numeric(),
        inc_mtbi_mem_daze = numeric(),
        inc_subc_v = numeric(),
        snf_scheme = numeric(),
        neuroimaging_domain = numeric(),
        variable_weighting = numeric(),
        pca = numeric(),
        output_vars = numeric(),
        eigen_or_rot = numeric(),
        stringsAsFactors = FALSE)
    row.names(design_matrix) <- NULL
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
#'
#' @return design_matrix New design matrix containing additional rows
#'
#' @export
add_design_matrix_rows <- function(design_matrix, nrows) {
    for (n in 1:nrows) {
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
        neuroimaging_domain <- sample(1:2, 1)
        variable_weighting <- sample(1:4, 1)
        pca <- sample(1:3, 1)
        output_vars <- sample(1:2, 1)
        eigen_or_rot <- sample(1:2, 1)
        # Putting it all together
        new_row <- cbind(
            row_id,
            inclusions,
            snf_scheme,
            neuroimaging_domain,
            variable_weighting,
            pca,
            output_vars,
            eigen_or_rot)
        # Appending to design matrix
        colnames(new_row) <- colnames(design_matrix)
        design_matrix <- rbind(design_matrix, data.frame(new_row))
    }
    row.names(design_matrix) <- NULL
    return(design_matrix)
}

#' Generate data_list object
#'
#' This is the major data object that will be processed when iterating through
#' the design matrix. The full list contains one list per measurement type.
#' Within each measurement type's list, elements include the actual data
#' structure, the name, the subdomain (if applicable), the domain, and the
#' data 'type' (i.e, numerical or categorical).
#'
#' @param mtbi_loc mtbi_loc dataframe generated by the `get_mtbi_loc`
#'  function
#' @param mtbi_mechanism mtbi_mechanisc dataframe generated by the
#'  `get_mtbi_mechanism` function
#' @param mtbi_mem_daze mtbi_mem_dazc dataframe generated by the
#'  `get_mtbi_mem_daze` function
#' @param income incomc dataframe generated by the `get_income` function
#' @param interview_age interview_agc dataframe generated by the
#'  `get_interview_age` function
#' @param mtbi_age mtbi_agc dataframe generated by the `get_mtbi_age` function
#' @param pubertal_status pubertal_statuc dataframe generated by the
#'  `get_pubertal_status` function
#' @param race race_cc dataframe generated by the `get_race` function
#' @param sex sec dataframe generated by the `get_sex` function
#' @param wmndf wmndc dataframe generated by the `get_wmndf` function
#' @param headaches headachec dataframe generated by the `get_headaches`
#'  function
#' @param mtbi_count mtbi_counc dataframe generated by the `get_mtbi_count`
#'  function
#' @param gord_cor gord_coc dataframe generated by the `get_gord_cor` function
#' @param gord_var gord_vac dataframe generated by the `get_gord_var` function
#' @param subc_cor subc_coc dataframe generated by the `get_subc_cor` function
#' @param subc_var subc_vac dataframe generated by the `get_subc_var` function
#' @param cort_sa cort_sc dataframe generated by the `get_cort_sa` function
#' @param cort_t cort_c dataframe generated by the `get_cort_t` function
#' @param subc_v subc_c dataframe generated by the `get_subc_v` function
#'
#' @export
generate_data_list <- function(mtbi_loc = NULL,
                               mtbi_mechanism = NULL,
                               mtbi_mem_daze = NULL,
                               income = NULL,
                               interview_age = NULL,
                               mtbi_age = NULL,
                               pubertal_status = NULL,
                               race = NULL,
                               sex = NULL,
                               wmndf = NULL,
                               headaches = NULL,
                               mtbi_count = NULL,
                               gord_cor = NULL,
                               gord_var = NULL,
                               subc_cor = NULL,
                               subc_var = NULL,
                               cort_sa = NULL,
                               cort_t = NULL,
                               subc_v = NULL) {
    mtbi_loc_list <-
        list(mtbi_loc, "mtbi_loc", "none", "as", "categorical")
    mtbi_mechanism_list <-
        list(mtbi_mechanism, "mtbi_mechanism", "none", "as", "categorical")
    mtbi_mem_daze_list <-
        list(mtbi_mem_daze, "mtbi_mem_daze", "none", "as", "categorical")
    income_list <-
        list(income, "income", "none", "d", "numeric")
    interview_age_list <-
        list(interview_age, "interview_age", "none", "d", "numeric")
    mtbi_age_list <-
        list(mtbi_age, "mtbi_age", "none", "d", "numeric")
    pubertal_status_list <-
        list(pubertal_status, "pubertal_status", "none", "d", "numeric")
    race_list <-
        list(race, "race", "none", "d", "categorical")
    sex_list <-
        list(sex, "sex", "none", "d", "categorical")
    headaches_list <-
        list(headaches, "headaches", "none", "mh", "categorical")
    mtbi_count_list <-
        list(mtbi_count, "mtbi_count", "none", "mh", "numeric")
    wmndf_list <-
        list(wmndf, "wmndf", "dmri", "n", "numeric")
    gord_cor_list <-
        list(gord_cor, "gord_cor", "rsfmri", "n", "numeric")
    gord_var_list <-
        list(gord_var, "gord_var", "rsfmri", "n", "numeric")
    subc_cor_list <-
        list(subc_cor, "subc_cor", "rsfmri", "n", "numeric")
    subc_var_list <-
        list(subc_var, "subc_var", "rsfmri", "n", "numeric")
    cort_sa_list <-
        list(cort_sa, "cort_sa", "smri", "n", "numeric")
    cort_t_list <-
        list(cort_t, "cort_t", "smri", "n", "numeric")
    subc_v_list <-
        list(subc_v, "subc_v", "smri", "n", "numeric")
    # The object that will contain all the data
    full_list <-
        list(mtbi_loc_list,
             mtbi_mechanism_list,
             mtbi_mem_daze_list,
             income_list,
             interview_age_list,
             mtbi_age_list,
             pubertal_status_list,
             race_list,
             sex_list,
             headaches_list,
             mtbi_count_list,
             wmndf_list,
             gord_cor_list,
             gord_var_list,
             subc_cor_list,
             subc_var_list,
             cort_sa_list,
             cort_t_list,
             subc_v_list)
    # Assign names to the nested list elements
    full_list_names <- c("data", "name", "subdomain", "domain", "type")
    full_list <- lapply(full_list, stats::setNames, full_list_names)
    # Only keep measurement types where object[[1]] (the data) was provided
    data_list <- Filter(function(x) !(is.null(x$"data")), full_list)
    return(data_list)
}

#' Print data_list nested dims
#'
#' @param data_list The data_list object to be reduced
#'
#' @export
print_data_list_dims <- function(data_list) {
    for (i in seq_along(data_list)) {
        print(dim(data_list[[i]][[1]]))
    }
}

#' Reduce data_list to common subjects
#'
#' Given a `data_list` object, reduce each nested dataframe to contain only the
#'  set of subjects that are shared by all nested dataframes
#'
#' @param data_list The data_list object to be reduced
#'
#' @return reduced_data_list The data_list object subsetted only to subjectssnf
#'  shared across all nested dataframes
#' @export
reduce_dl_to_common <- function(data_list) {
    subjects <- lapply(data_list, function(x) x[[1]]$"subjectkey")
    data_objects <- lapply(data_list, function(x) x[[1]])
    common_subjects <- Reduce(intersect, subjects)
    filtered_data_objects <-
        lapply(data_objects,
        function(x) {
            dplyr::filter(x, x$"subjectkey" %in% common_subjects)
        })
    reduced_data_list <- data_list
    for (i in seq_along(data_list)) {
        reduced_data_list[[i]][[1]] <- filtered_data_objects[[i]]
    }
    return(reduced_data_list)
}

#' Given a data_list object, sort data elements by subjectkey
#'
#' @param data_list The data_list object to be arranged
#'
#' @return arranged_data_list The arranged data_list object
#'
#' @export
arrange_dl <- function(data_list) {
    data_objects <- lapply(data_list, function(x) x[[1]])
    arranged_data_objects <-
        lapply(data_objects,
        function(x) {
            dplyr::arrange(x, x$"subjectkey")
        })
    arranged_data_list <- data_list
    for (i in seq_along(data_list)) {
        arranged_data_list[[i]][[1]] <- arranged_data_objects[[i]]
    }
    return(arranged_data_list)
}

remove_dl_nas <- function(data_list) {
    num_nas <- rapply(data_list, function(x) is.na(x))
    sum(num_nas)
    na_free_data_list <- 0
    return(na_free_data_list)
}


#' SNF a data_list
#'
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#' @param scheme Which SNF system to use to achieve the final fused network
#'
#' @return df_list This will need to become the SNF's results
#'
#' @export
snf_step <- function(data_list, scheme) {
    # Subset just to those patients who are common in all inputs
    data_list <- data_list |>
        reduce_dl_to_common() |>
        arrange_dl()
    # Remove NAs function can go here later
    if (scheme == "individual") {
        dist_list <- lapply(data_list,
            function(x) {
                get_dist_matrix(df = x$"data", input_type = x$"type")
            })
        sim_list <- lapply(dist_list,
            function(x) {
                SNFtool::affinityMatrix(x)
            })
        fused_network <- SNFtool::SNF(sim_list)
    } else if (scheme == "domain") {
        # untested
        data_list <- domain_merge(data_list)
        dist_list <- lapply(data_list,
            function(x) {
                get_dist_matrix(df = x$"data", input_type = x$"type")
            })
        sim_list <- lapply(dist_list,
            function(x) {
                SNFtool::affinityMatrix(x)
            })
        fused_network <- SNFtool::SNF(sim_list)
    } else if (scheme == "subdomain") {
        #continue from here
    }
    return(fused_network)
}


#' Execute variations of SNF as described by a design matrix
#'
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#' @param design_matrix matrix indicating parameters to iterate SNF through
#'
#' @return populated_design_matrix design matrix with filled columns related to
#'  subtype membership and cluster performance
#'
#' @export
execute_design_matrix <- function(data_list, design_matrix) {

    populated_design_matrix <- 0
    return(populated_design_matrix)
}

#' Select inputs from a data_list
#'
#' @description
#' Given a data list and a design matrix row, returns a data list of selected
#'  inputs
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#' @param design_matrix matrix indicating parameters to iterate SNF through
#'
#' @return selected_data_list
#'
#' @export
execute_inclusion <- function(data_list, design_matrix) {
# Dataframe just of the inclusion variables
    inc_df <- design_matrix |>
        dplyr::select(dplyr::starts_with("inc"))
    # The subset of columns that are in 'keep' (1) mode
    keepcols <- colnames(inc_df)[inc_df[1, ] == 1]
    # The list of data_list elements that are to be selected
    in_keeps_list <- lapply(data_list,
        function(x) {
            paste0("inc_", x$"name") %in% keepcols
        })
    # Converting to a logical type to do the selection
    in_keeps_log <- c(unlist(in_keeps_list))
    # The selection
    selected_df <- data_list[in_keeps_log]
    return(selected_df)
}

#' Summarize a data list
#'
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#'
#' @return dl_summary Summarized output
#'
#' @export
sdl <- function(data_list) {
    dl_summary <-
        data.frame(
            name = unlist(lapply(data_list, function(x) x$"name")),
            domain = unlist(domains(data_list)),
            length = unlist(lapply(data_list, function(x) dim(x$"data")[1])),
            width = unlist(lapply(data_list, function(x) dim(x$"data")[2])))
    return(dl_summary)
}

#' Return domains of a data list
#'
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#'
#' @return domain_list list of domains
#'
#' @export
domains <- function(data_list) {
    domain_list <- lapply(data_list, function(x) x$"domain")
    return(domain_list)
}

#' Return subdomains of a data list
#'
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#'
#' @return subdomain_list list of subdomains
#'
#' @export
subdomains <- function(data_list) {
    subdomain_list <- lapply(data_list, function(x) x$"subdomain")
    return(subdomain_list)
}


#' Merge data_list by domain
#'
#' @param data_list nested list of input data generated by the function
#'  `get_data_list()`
#'
#' @return domain_dl
#'
#' @export
domain_merge <- function(data_list) {
    domain_dl <- list()
    for (i in seq_along(data_list)) {
        current_component <- data_list[[i]]
        current_name <- data_list[[i]]$"name"
        current_domain <- data_list[[i]]$"domain"
        if (length(domain_dl) == 0) {
            print(paste0("Adding component #", i, ": ",
                         current_name, ", ",
                         "domain: ",
                         current_domain))
            domain_dl <- append(domain_dl, list(current_component))
        } else if (current_domain %in% domains(domain_dl)) {
            print(paste0("Merging component #", i, ": ",
                         current_name, ", ",
                         "domain: ",
                         current_domain))
            existing_match_pos <- which(domains(domain_dl) == current_domain)
            existing_component <- domain_dl[[existing_match_pos]]
            existing_match_data <- existing_component$"data"
            data_to_merge <- current_component$"data"
            merged_data <- dplyr::inner_join(
                existing_match_data, data_to_merge, by = "subjectkey")
            merged_component <- existing_component
            merged_component$"data" <- merged_data
            merged_component$"name" <-
                paste0("merged_", merged_component$"domain")
            merged_component$"type" <- dplyr::case_when(
                existing_component$"type" == current_component$"type" ~
                    current_component$"type",
                existing_component$"type" != current_component$"type" ~
                    "mixed"
            )
            domain_dl[[existing_match_pos]] <- merged_component
        } else {
            print(paste0("Adding component #", i, ": ",
                         current_name, ", ",
                         "domain: ",
                         current_domain))
            domain_dl <- append(domain_dl, list(current_component))
        }
    }
    return(domain_dl)
}
