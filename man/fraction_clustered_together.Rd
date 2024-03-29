% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stability_measures.R
\name{fraction_clustered_together}
\alias{fraction_clustered_together}
\title{Average fraction of clustering together}
\usage{
fraction_clustered_together(
  data_list_subsamples,
  settings_matrix,
  solutions_matrix
)
}
\arguments{
\item{data_list_subsamples}{A list of subsets of a larger data_list. See
?subsample_data_list to obtain this object.}

\item{settings_matrix}{A settings_matrix.}

\item{solutions_matrix}{A solutions_matrix.}
}
\value{
fraction_together_df Dataframe containing the average fraction
of continued clustering together for all supplied solutions
}
\description{
Calculate average fraction of times that patients who clusted
together in the full solution continued to cluster together in all the
subsampled solutions.
}
