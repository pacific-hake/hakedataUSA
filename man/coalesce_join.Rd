% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coalesce_join.R
\name{coalesce_join}
\alias{coalesce_join}
\title{Mutating join}
\usage{
coalesce_join(
  x,
  y,
  by = NULL,
  suffix = c(".x", ".y"),
  join = dplyr::full_join,
  ...
)
}
\arguments{
\item{x, y}{A pair of data frames, data frame extensions (e.g. a tibble), or
lazy data frames (e.g. from dbplyr or dtplyr). See \emph{Methods}, below, for
more details.}

\item{by}{A join specification created with \code{\link[dplyr:join_by]{join_by()}}, or a character
vector of variables to join by.

If \code{NULL}, the default, \verb{*_join()} will perform a natural join, using all
variables in common across \code{x} and \code{y}. A message lists the variables so
that you can check they're correct; suppress the message by supplying \code{by}
explicitly.

To join on different variables between \code{x} and \code{y}, use a \code{\link[dplyr:join_by]{join_by()}}
specification. For example, \code{join_by(a == b)} will match \code{x$a} to \code{y$b}.

To join by multiple variables, use a \code{\link[dplyr:join_by]{join_by()}} specification with
multiple expressions. For example, \code{join_by(a == b, c == d)} will match
\code{x$a} to \code{y$b} and \code{x$c} to \code{y$d}. If the column names are the same between
\code{x} and \code{y}, you can shorten this by listing only the variable names, like
\code{join_by(a, c)}.

\code{\link[dplyr:join_by]{join_by()}} can also be used to perform inequality, rolling, and overlap
joins. See the documentation at \link[dplyr:join_by]{?join_by} for details on
these types of joins.

For simple equality joins, you can alternatively specify a character vector
of variable names to join by. For example, \code{by = c("a", "b")} joins \code{x$a}
to \code{y$a} and \code{x$b} to \code{y$b}. If variable names differ between \code{x} and \code{y},
use a named character vector like \code{by = c("x_a" = "y_a", "x_b" = "y_b")}.

To perform a cross-join, generating all combinations of \code{x} and \code{y}, see
\code{\link[dplyr:cross_join]{cross_join()}}.}

\item{suffix}{If there are non-joined duplicate variables in \code{x} and
\code{y}, these suffixes will be added to the output to disambiguate them.
Should be a character vector of length 2.}

\item{join}{The {dplyr} function, or function from any other package, that
should be used to join \code{x} and \code{y}. The default is to perform a full join,
i.e., \code{\link[dplyr:mutate-joins]{dplyr::full_join()}} between the two data frames.}

\item{...}{Any additional arguments you wish to supply to the function
specified in \code{join}.}
}
\description{
A join that adds information from matching columns from \code{y} to \code{x}. If the
value in \code{y} is \code{NA}, then the value from \code{x} will be used. Thus, all new
information can be used to overwrite the old information.
}
\references{
\url{https://alistaire.rbind.io/blog/coalescing-joins/}
}
\author{
Edward Visel with some changes from Kelli F. Johnson.
}
