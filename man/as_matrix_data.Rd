% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_matrix_data.R
\name{as_matrix_data}
\alias{as_matrix_data}
\alias{as_matrix_data.matrix}
\alias{as_matrix_data.data.frame}
\alias{as_matrix_data.correlate}
\alias{as_matrix_data.default}
\title{as_matrix_data}
\usage{
as_matrix_data(x, ...)

\method{as_matrix_data}{matrix}(x, name = NULL, ...)

\method{as_matrix_data}{data.frame}(x, name = NULL, include = "numeric", ...)

\method{as_matrix_data}{correlate}(x, ...)

\method{as_matrix_data}{default}(x, ...)
}
\arguments{
\item{x}{any \code{R} object.}

\item{...}{ignore.}

\item{name}{variable name.}

\item{include}{one of "numeric" (default), "character" or "factor".}
}
\value{
a matrix_data object.
}
\description{
Coerce to a matrix_data
}
\author{
Hou Yun
}
