% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correlate.R
\name{correlate}
\alias{correlate}
\alias{fast_correlate}
\alias{fast_correlate2}
\title{Correlate}
\usage{
correlate(
  x,
  y = NULL,
  method = "pearson",
  use = "everything",
  adjust = FALSE,
  adjust_method = "holm",
  ...
)

fast_correlate(
  x,
  y = NULL,
  adjust = FALSE,
  adjust_method = "holm",
  use = "everything",
  ...
)

fast_correlate2(
  x,
  method = "pearson",
  adjust = FALSE,
  adjust_method = "holm",
  ...
)
}
\arguments{
\item{x, y}{a matrix object or NULL.}

\item{method}{a character string indicating which correlation coefficient is to be used
for the test. One of "pearson", "kendall", or "spearman".}

\item{use}{an optional character string giving a method for computing covariances in the presence of missing values.}

\item{adjust}{logical, if TRUE (default) will adjust p value for multiple comparisons.}

\item{adjust_method}{correction method.}

\item{...}{extra params, see Details.}
}
\value{
a list with correlation matrix, P values matrix, confidence intervals matrix.
}
\description{
Matrix of Correlations, P-values and confidence intervals
}
\details{
The columns of 'x' will be tested for each pair when y is NULL(the default),
otherwise each column in 'x' and each column in 'y' is tested for each pair.
}
\examples{
correlate(mtcars)

m1 <- matrix(rnorm(100), nrow = 10)
m2 <- matrix(rnorm(60), nrow = 10)
correlate(m1, m2)

## adjust p value
correlate(m1, m2, cor.test = TRUE, adjust = TRUE)

## fast compute correlation
\dontrun{
require(WGCNA)
fast_correlate(m1, m2)

require(picante)
  fast_correlate2(m1)
}
}
\seealso{
\code{\link[stats]{cor}}, \code{\link[stats]{cor.test}}.
}
\author{
Hou Yun
}
