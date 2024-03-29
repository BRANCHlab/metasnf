% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correlation_plotting.R
\name{corrHeatmap}
\alias{corrHeatmap}
\title{Generate correlation heatmap}
\usage{
corrHeatmap(corr, row_km, column_km, labels_colour = NULL)
}
\arguments{
\item{corr}{matrix of outcomes-outcomes correlation p_values}

\item{row_km}{kmean partitioning of features along rows for display}

\item{column_km}{kmean partitioning of features along columns for display}

\item{labels_colour}{optional argument to specify color labels for datatypes}
}
\value{
hm a correlation heatmap
}
\description{
Generate correlation heatmap
}
