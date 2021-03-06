% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validation_formating_tools.r
\name{formatDataFrameLK}
\alias{formatDataFrameLK}
\title{Format a data.frame for LabKey}
\usage{
formatDataFrameLK(df, long.int.as.char = TRUE, long = 10^9)
}
\arguments{
\item{df}{A data.frame. An element from the `datasets` list.}

\item{long.int.as.char}{If TRUE, long integers will be uploaded as character variables.}

\item{long}{The threshold to consider an integer to be long long.}
}
\description{
Format a data.frame for LabKey
}

