% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_catch-.R
\name{process_catch_pacfin}
\alias{process_catch_pacfin}
\title{Print summaries and figures for catches from PacFIN}
\usage{
process_catch_pacfin(
  pcatch = get_local(file = "pacfin_catch.Rdat"),
  nyears = 5,
  savedir = hakedata_wd()
)
}
\arguments{
\item{pcatch}{An R object of PacFIN catches.
The default value is loaded from the disk using \code{\link[=get_local]{get_local()}}.
Otherwise, the R object is typically stored in the \code{hakedata} environment.}

\item{nyears}{An integer specifying the number of years you want to plot,
where the most recent year will be sequenced back in time \code{nyears}.
The default is typically five years.}

\item{savedir}{A full path to a directory where you want files saved.
The default is to save them in a folder found using \code{\link[=hakedata_wd]{hakedata_wd()}},
which returns a path specific to the personal preferences of JTC members.}
}
\value{
The following files are saved to the disk:
\itemize{
\item us-shore-catch-by-month.csv
\item us-research-catch-by-month.csv
\item us-ti-catch-by-month.csv
}
}
\description{
Print summaries and figures for catches from PacFIN
}
