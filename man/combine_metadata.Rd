% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combine_matadata.R
\name{combine_metadata}
\alias{combine_metadata}
\title{Combine table and metadata}
\usage{
combine_metadata(table, metadata, convert = FALSE)
}
\arguments{
\item{table}{tibble}

\item{metadata}{tibble}

\item{convert}{logical}
}
\value{
tibble
}
\description{
Combine table and metadata
}
\examples{
data(metadata)
data(ko_abundance)
combine_table <- combine_metadata(ko_abundance, metadata, convert = TRUE)
}
