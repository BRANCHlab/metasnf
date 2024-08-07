% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster_extraction.R
\name{get_clusters}
\alias{get_clusters}
\title{Extract cluster membership vector from one solutions matrix row}
\usage{
get_clusters(solutions_matrix_row)
}
\arguments{
\item{solutions_matrix_row}{Output matrix row}
}
\value{
clusters list of assigned clusters
}
\description{
This function takes in a single row of a solutions matrix and returns a
vector containing the cluster assignments for each observation. It is
similar to \code{get_cluster_df()}, which takes a solutions matrix with only one
row and returns a dataframe with two columns: "cluster" and "subjectkey"
'(the UID of the observation) and \code{get_cluster_solutions()}, which takes a
solutions matrix with any number of rows and returns a dataframe indicating
the cluster assignments for each of those rows.
}
