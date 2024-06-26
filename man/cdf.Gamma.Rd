% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gamma.R
\name{cdf.Gamma}
\alias{cdf.Gamma}
\title{Evaluate the cumulative distribution function of a Gamma distribution}
\usage{
\method{cdf}{Gamma}(d, x, drop = TRUE, elementwise = NULL, ...)
}
\arguments{
\item{d}{A `Gamma` object created by a call to [Gamma()].}

\item{x}{A vector of elements whose cumulative probabilities you would
like to determine given the distribution `d`.}

\item{drop}{logical. Should the result be simplified to a vector if possible?}

\item{elementwise}{logical. Should each distribution in \code{d} be evaluated
at all elements of \code{x} (\code{elementwise = FALSE}, yielding a matrix)?
Or, if \code{d} and \code{x} have the same length, should the evaluation be
done element by element (\code{elementwise = TRUE}, yielding a vector)? The
default of \code{NULL} means that \code{elementwise = TRUE} is used if the
lengths match and otherwise \code{elementwise = FALSE} is used.}

\item{...}{Arguments to be passed to \code{\link[stats]{pgamma}}.
Unevaluated arguments will generate a warning to catch mispellings or other
possible errors.}
}
\value{
In case of a single distribution object, either a numeric
  vector of length `probs` (if `drop = TRUE`, default) or a `matrix` with
  `length(x)` columns (if `drop = FALSE`). In case of a vectorized distribution
  object, a matrix with `length(x)` columns containing all possible combinations.
}
\description{
Evaluate the cumulative distribution function of a Gamma distribution
}
\examples{

set.seed(27)

X <- Gamma(5, 2)
X

random(X, 10)

pdf(X, 2)
log_pdf(X, 2)

cdf(X, 4)
quantile(X, 0.7)

cdf(X, quantile(X, 0.7))
quantile(X, cdf(X, 7))
}
