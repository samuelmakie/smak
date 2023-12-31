% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as.Date2.R
\name{as.Date2}
\alias{as.Date2}
\title{as.Date2}
\usage{
as.Date2(
  x,
  format,
  tryFormats = c("\%Y-\%m-\%d", "\%Y/\%m/\%d", "\%d-\%m-\%Y", "\%d/\%m/\%Y")
)
}
\arguments{
\item{x}{A vector}

\item{format}{See \link{as.Date}}

\item{tryFormats}{See \link{as.Date}}
}
\value{
A date vector
}
\description{
A modified \link[base:as.Date]{base::as.Date} such that the function still runs and Nreturns NA if coercion wasn't successful.
}
