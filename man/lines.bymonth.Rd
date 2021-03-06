% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_catchvmonthbyyear.R
\name{lines.bymonth}
\alias{lines.bymonth}
\title{Plot a line of time-series data}
\usage{
\method{lines}{bymonth}(x, y, plotType = c("default", "proportion", "cumulative"), ...)
}
\arguments{
\item{x}{A vector of months in numeric form.
Also, commonly known as the \code{period}.}

\item{y}{Dependent variable in a vector that is the same length as \code{x}.
Typically, this will be monthly catches for a given year.}

\item{plotType}{The type of plot you wish to create, where the y-axis
type will change based on the entry. The default of \code{default} will lead
to the raw data being plotted. \code{proportion} plots the proportion and
\code{cumulative} plots the cumulative value for that year.}

\item{...}{Arguments passed to \link[graphics:lines]{graphics::lines}}
}
\value{
A line of the dependent variable by month
is added to the current figure.
}
\description{
Plot a line of time-series data by month.
}
