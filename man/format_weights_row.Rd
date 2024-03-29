% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weights.R
\name{format_weights_row}
\alias{format_weights_row}
\title{Prep weights_row for matrix multiplication}
\usage{
format_weights_row(weights_row)
}
\arguments{
\item{weights_row}{Vector (possibly size 1) of weights.}
}
\description{
Wrapper around the diag() function. This function was created because diag()
applied to a scalar doesn't spit that scalar back out, but instead returns
identity matrix of dimension (scalar * scalar).
}
