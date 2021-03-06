% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_table_wheaders.R
\name{read_table_wheaders}
\alias{read_table_wheaders}
\title{Read table with headers}
\usage{
read_table_wheaders(doc, sep = "\\t", na.string)
}
\arguments{
\item{doc}{document containing data}

\item{sep}{optional separator. Default is tab}

\item{na.string}{optional na.string}
}
\value{
final_data resulting data table
}
\description{
Reads in a data file with a large header commented with #'s.  fread command did not work for large headers
}
