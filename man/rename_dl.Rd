% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_list.R
\name{rename_dl}
\alias{rename_dl}
\title{Rename features in a data_list}
\usage{
rename_dl(data_list, name_mapping)
}
\arguments{
\item{data_list}{A nested list of input data from \code{generate_data_list()}.}

\item{name_mapping}{A named vector where the values are the features to be
renamed and the names are the new names for those features.}
}
\description{
Rename features in a data_list
}
\examples{

library(metasnf)

data_list <- generate_data_list(
    list(pubertal, "pubertal_status", "demographics", "continuous"),
    list(anxiety, "anxiety", "behaviour", "ordinal"),
    list(depress, "depressed", "behaviour", "ordinal"),
    uid = "unique_id"
)

summarize_dl(data_list, "feature")

name_changes <- c(
    "anxiety_score" = "cbcl_anxiety_r",
    "depression_score" = "cbcl_depress_r"
)

data_list <- rename_dl(data_list, name_changes)

summarize_dl(data_list, "feature")
}
