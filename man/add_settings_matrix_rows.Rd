% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/settings_matrix.R
\name{add_settings_matrix_rows}
\alias{add_settings_matrix_rows}
\title{Add settings matrix rows}
\usage{
add_settings_matrix_rows(
  settings_matrix,
  seed = NULL,
  nrows = 0,
  min_removed_inputs = 0,
  max_removed_inputs = sum(startsWith(colnames(settings_matrix), "inc_")) - 1,
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
  retry_limit = 10
)
}
\arguments{
\item{settings_matrix}{The existing settings matrix}

\item{seed}{set a seed for the random matrix generation. Setting this value
will change the seed of the global environment.}

\item{nrows}{Number of rows to generate for the settings matrix.}

\item{min_removed_inputs}{The smallest number of input dataframes that may be
randomly removed. By default, 0.}

\item{max_removed_inputs}{The largest number of input dataframes that may be
randomly removed. By default, this is 1 less than all the provided input
dataframes in the data_list.}

\item{dropout_dist}{Parameter controlling how the random removal of input
dataframes should occur. Can be "none" (no input dataframes are randomly
removed), "uniform" (uniformly sample between min_removed_inputs and max_removed_inputs
to determine number of input dataframes to remove), or "exponential" (pick
number of input dataframes to remove by sampling from min_removed_inputs to
max_removed_inputs with an exponential distribution; default).}

\item{min_alpha}{The minimum value that the alpha hyperparameter can have.
Random assigned value of alpha for each row will be obtained by uniformly
sampling numbers between \code{min_alpha} and \code{max_alpha} at intervals of 0.1.
Cannot be used in conjunction with the \code{alpha_values} parameter.}

\item{max_alpha}{The maximum value that the alpha hyperparameter can have.
See \code{min_alpha} parameter. Cannot be used in conjunction with the
\code{alpha_values} parameter.}

\item{min_k}{The minimum value that the k hyperparameter can have.
Random assigned value of k for each row will be obtained by uniformly
sampling numbers between \code{min_k} and \code{max_k} at intervals of 1.
Cannot be used in conjunction with the \code{k_values} parameter.}

\item{max_k}{The maximum value that the k hyperparameter can have.
See \code{min_k} parameter. Cannot be used in conjunction with the
\code{k_values} parameter.}

\item{min_t}{The minimum value that the t hyperparameter can have.
Random assigned value of t for each row will be obtained by uniformly
sampling numbers between \code{min_t} and \code{max_t} at intervals of 1.
Cannot be used in conjunction with the \code{t_values} parameter.}

\item{max_t}{The maximum value that the t hyperparameter can have.
See \code{min_t} parameter. Cannot be used in conjunction with the
\code{t_values} parameter.}

\item{alpha_values}{A number or numeric vector of a set of possible values
that alpha can take on. Value will be obtained by uniformly sampling the
vector. Cannot be used in conjunction with the \code{min_alpha} or \code{max_alpha}
parameters.}

\item{k_values}{A number or numeric vector of a set of possible values
that k can take on. Value will be obtained by uniformly sampling the
vector. Cannot be used in conjunction with the \code{min_k} or \code{max_k}
parameters.}

\item{t_values}{A number or numeric vector of a set of possible values
that t can take on. Value will be obtained by uniformly sampling the
vector. Cannot be used in conjunction with the \code{min_t} or \code{max_t}
parameters.}

\item{possible_snf_schemes}{A vector containing the possible snf_schemes to
uniformly randomly select from. By default, the vector contains all
3 possible schemes: c(1, 2, 3). 1 corresponds to the "individual" scheme,
2 corresponds to the "domain" scheme, and 3 corresponds to the "twostep"
scheme.}

\item{clustering_algorithms}{A list of clustering algorithms to uniformly
randomly pick from when clustering. When not specified, randomly select
between spectral clustering using the eigen-gap heuristic and spectral
clustering using the rotation cost heuristic. See ?generate_clust_algs_list
for more details on running custom clustering algorithms.}

\item{continuous_distances}{A vector of continuous distance metrics to use
when a custom distance_metrics_list is provided.}

\item{discrete_distances}{A vector of categorical distance metrics to use
when a custom distance_metrics_list is provided.}

\item{ordinal_distances}{A vector of categorical distance metrics to use
when a custom distance_metrics_list is provided.}

\item{categorical_distances}{A vector of categorical distance metrics to use
when a custom distance_metrics_list is provided.}

\item{mixed_distances}{A vector of mixed distance metrics to use
when a custom distance_metrics_list is provided.}

\item{distance_metrics_list}{List containing distance metrics to vary over.
See ?generate_distance_metrics_list.}

\item{snf_input_weights}{Nested list containing weights for when SNF is
used to merge individual input measures (see ?generate_snf_weights)}

\item{snf_domain_weights}{Nested list containing weights for when SNF is
used to merge domains (see ?generate_snf_weights)}

\item{retry_limit}{The maximum number of attempts to generate a novel row.
This function does not return matrices with identical rows. As the range of
requested possible settings tightens and the number of requested rows
increases, the risk of randomly generating a row that already exists
increases. If a new random row has matched an existing row \code{retry_limit}
number of times, the function will terminate.}
}
\value{
settings_matrix A settings matrix
}
\description{
Add settings matrix rows
}
