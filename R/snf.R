# Given list of similarity matrices, return fused network
snf <- function() {
    return(NULL)
}

# Given measurement types, their subdomains, and their domains
snf_step <- function() {
    return(NULL)
}

# Given fused network, return cluster membership of patients
spectral_clustering <- function() {
    return(NULL)
}

# Given clustered subjects and outcome measures, evaluate clustering utility
evaluate_clustering <- function() {
    return(NULL)
}


#' Construct the base of the design matrix
#'
#' @return design_matrix A skeleton dataframe to build a design matrix out of
#'
#' @export
build_design_matrix_base <- function() {
    design_matrix <- data.frame(
        inc_mtbi_age = numeric(),
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
        stringsAsFactors = FALSE)
    return(design_matrix)
}

#' Generate random sequence for column exclusions component of design matrix
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

#' Add and populate rows to an existing design matrix
#'
#' @param design_matrix The existing design matrix
#' @param nrows The number of rows to be added to the design matrix
#'
#' @return design_matrix New design matrix containing additional rows
#'
#' @export
add_design_matrix_rows <- function(design_matrix, nrows) {
    for (n in 1:nrows) {
        new_row <- vector()
        num_inclusion_cols <- sum(startsWith(colnames(design_matrix), "inc"))
        new_row <- random_removal(num_inclusion_cols)
        new_row <- t(data.frame(new_row))
        colnames(new_row) <- colnames(design_matrix)
        design_matrix <- rbind(design_matrix, t(t(data.frame(new_row))))
        print(n)
        print(design_matrix)
    }
    return(design_matrix)
}

iterate_design_matrix <- function() {
    design_matrix <- build_design_matrix_base()
    # build design matrix rows
    # for each option, randomly select a value
    # check if that vector of options has been done before
    # feed those options into SNF
  # execute design matrix rows
  # update design matrix output columns
  return(design_matrix)
}

visualize_design_matrix <- function() {
  return(NULL)
}
