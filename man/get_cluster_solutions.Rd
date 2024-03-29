% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/characterization.R
\name{get_cluster_solutions}
\alias{get_cluster_solutions}
\title{Extract all cluster solutions from a solutions_matrix}
\usage{
get_cluster_solutions(solutions_matrix)
}
\arguments{
\item{solutions_matrix}{A solutions_matrix.}
}
\value{
cluster_solutions A dataframe where each row is a patient and each
column is a different run of SNF stored in the solutions_matrix. Values
along the columns are the cluster that each patient was assigned to.
}
\description{
Extract all cluster solutions from a solutions_matrix
}
