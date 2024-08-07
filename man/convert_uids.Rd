% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_list.R
\name{convert_uids}
\alias{convert_uids}
\title{Convert unique identifiers of data_list to 'subjectkey'}
\usage{
convert_uids(data_list, uid = NULL)
}
\arguments{
\item{data_list}{A nested list of input data from \code{generate_data_list()}.}

\item{uid}{(string) the name of the uid column currently used data}
}
\value{
dl_renamed_id data list with 'subjectkey' as UID
}
\description{
Column name "subjectkey" is reserved for the unique identifier of subjects.
This function ensures all dataframes have their UID set as "subjectkey".
}
