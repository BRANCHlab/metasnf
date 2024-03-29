% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manhattan_plot.R
\name{manhattan_plot}
\alias{manhattan_plot}
\title{Manhattan plot of p-values}
\usage{
manhattan_plot(
  data,
  key_mode = FALSE,
  threshold = NULL,
  bonferroni_line = FALSE
)
}
\arguments{
\item{data}{Output from either the pval_select() function when key_mode is
FALSE or from the calculate_associations() function when key_mode is TRUE}

\item{key_mode}{When TRUE, generates a plot of associations relative to a
key variable (calculated previously through calculate_associations) rather
than associations against a cluster solution}

\item{threshold}{P-value threshold to plot dashed line at.}

\item{bonferroni_line}{If TRUE, plots a dashed black line at the
Bonferroni-corrected equivalent of the p-value threshold.}
}
\description{
Manhattan plot of p-values
}
