% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{suff_stat}
\alias{suff_stat}
\title{Compute the sufficient statistics of a distribution from data}
\usage{
suff_stat(d, x, ...)
}
\arguments{
\item{d}{An object. The package provides methods for distribution
objects such as those from [Normal()] or [Binomial()] etc.}

\item{x}{A vector of data to compute the likelihood.}

\item{...}{Arguments passed to methods. Unevaluated arguments will generate a warning to
catch mispellings or other possible errors.}
}
\value{
a named list of sufficient statistics
}
\description{
Generic function for computing the sufficient statistics of
a distribution based on empirical data.
}
\examples{
X <- Normal()
suff_stat(X, c(-1, 0, 0, 0, 3))
}
