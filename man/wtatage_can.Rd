% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wtatage_can.R
\name{wtatage_can}
\alias{wtatage_can}
\title{Make \code{.csv} File of Canadian Weight-At-Age Data}
\usage{
wtatage_can(file_in, file_out = NULL, verbose = FALSE)
}
\arguments{
\item{file_in}{An absolute or relative path to the input file.}

\item{file_out}{An absolute or relative path name to save the
output data frame to the disk.}

\item{verbose}{A logical value specifying if output should be
printed to the screen for debugging. The default is \code{TRUE}.}
}
\value{
A data frame of Canadian weight-at-age values with acoustic
data post the 2017 removed, data from the research tows removed,
and fishery data landed during January-March removed.
}
\description{
Take input data provided by the Canadian JTC and make
a non-duplicated file to be read in for weight-at-age
calculations.
}
\author{
Kelli Faye Johnson
}
