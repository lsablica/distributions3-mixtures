% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Binomial.R
\name{pdf.Binomial}
\alias{pdf.Binomial}
\alias{log_pdf.Binomial}
\title{Evaluate the probability mass function of a Binomial distribution}
\usage{
\method{pdf}{Binomial}(d, x, drop = TRUE, elementwise = NULL, ...)

\method{log_pdf}{Binomial}(d, x, drop = TRUE, elementwise = NULL, ...)
}
\arguments{
\item{d}{A `Binomial` object created by a call to [Binomial()].}

\item{x}{A vector of elements whose probabilities you would like to
determine given the distribution `d`.}

\item{drop}{logical. Should the result be simplified to a vector if possible?}

\item{elementwise}{logical. Should each distribution in \code{d} be evaluated
at all elements of \code{x} (\code{elementwise = FALSE}, yielding a matrix)?
Or, if \code{d} and \code{x} have the same length, should the evaluation be
done element by element (\code{elementwise = TRUE}, yielding a vector)? The
default of \code{NULL} means that \code{elementwise = TRUE} is used if the
lengths match and otherwise \code{elementwise = FALSE} is used.}

\item{...}{Arguments to be passed to \code{\link[stats]{dbinom}}.
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
Evaluate the probability mass function of a Binomial distribution
}
\examples{

set.seed(27)

X <- Binomial(10, 0.2)
X

mean(X)
variance(X)
skewness(X)
kurtosis(X)

random(X, 10)

pdf(X, 2L)
log_pdf(X, 2L)

cdf(X, 4L)
quantile(X, 0.7)

cdf(X, quantile(X, 0.7))
quantile(X, cdf(X, 7))
}
