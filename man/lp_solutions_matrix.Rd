% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/label_propagation.R
\name{lp_solutions_matrix}
\alias{lp_solutions_matrix}
\title{Label propagate cluster solutions to unclustered subjects}
\usage{
lp_solutions_matrix(
  train_solutions_matrix,
  full_data_list,
  distance_metrics_list = NULL,
  weights_matrix = NULL,
  verbose = FALSE
)
}
\arguments{
\item{train_solutions_matrix}{A solutions_matrix derived from the training
set. The propagation algorithm is slow and should be used for validating a
top or top few meaningful chosen clustering solutions. It is advisable to
use only a small subset of rows from the original training solutions_matrix
for label propagation.}

\item{full_data_list}{A data_list containing subjects from both the training
and testing sets.}

\item{distance_metrics_list}{Like above - the distance_metrics_list (if any)
that was used for the original batch_snf call.}

\item{weights_matrix}{Like above.}

\item{verbose}{If TRUE, print progress to console.}
}
\value{
labeled_df a dataframe containing a column for subjectkeys,
a column for whether the subject was in the train (original) or test (held
out) set, and one column per row of the solutions matrix indicating the
original and propagated clusters.
}
\description{
Given a solutions_matrix derived from training subjects and a full_data_list
containing both training and test subjects, re-run SNF to generate a total
affinity matrix of both train and subjects and use the label propagation
algorithm to assigned predicted clusters to test subjects.
}
