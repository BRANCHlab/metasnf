% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{subs}
\alias{subs}
\title{Select all columns of a dataframe starting with a given string prefix.}
\usage{
subs(df)
}
\arguments{
\item{df}{Dataframe}
}
\value{
df_subs Dataframe with only 'subject_' prefixed columns
}
\description{
Removes the columns that are not prefixed with 'subject_' prefixed columns
from a dataframe. Useful intermediate step for extracting subject UIDs from
an solutions_matrix structure.
}
