% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/solutions_matrix.R
\name{lin_reg_p}
\alias{lin_reg_p}
\title{Linear regression p-value}
\usage{
lin_reg_p(clust_membership, outcome_df, outcome_var)
}
\arguments{
\item{clust_membership}{Dataframe of cluster membership (get_clustered_subs)}

\item{outcome_df}{Dataframe containing DV}

\item{outcome_var}{DV as a string}
}
\value{
p_val The overall p-value distinguishing clusters by the DV
}
\description{
Returns the p-value following an linear regression in which cluster
is the IV and a provided ordinal variable is the DV.
}
