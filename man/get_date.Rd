% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_date.R
\name{get_date}
\alias{get_date}
\title{Format a date to  a specific form}
\usage{
get_date(data, formatout = c("\%m", "\%Y"))
}
\arguments{
\item{data}{A vector of dates with class \code{POSIXct}.}

\item{formatout}{A character value specifying the format you want
the output to be in. See \link[base:strptime]{base::strptime} for options.}
}
\value{
A vector of numeric objects, such as months.
}
\description{
Format a date object into a specific numeric form.
}
