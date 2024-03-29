% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{batch_snf_time_remaining}
\alias{batch_snf_time_remaining}
\title{Time remaining until batch_snf completion}
\usage{
batch_snf_time_remaining(
  seconds_per_row,
  rows_remaining,
  row,
  remaining_seconds_vector
)
}
\arguments{
\item{seconds_per_row}{Integer in seconds of time taken for most recent SNF
'run}

\item{rows_remaining}{Number of rows left to complete in the settings matrix}

\item{row}{Current row in the settings matrix}

\item{remaining_seconds_vector}{Vector storing up to the 10 most recent
row completion times}
}
\value{
remaining_seconds_vector Updated remaining_seconds_vector
}
\description{
Time remaining until batch_snf completion
}
