% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Bernoulli.R
\name{support.Bernoulli}
\alias{support.Bernoulli}
\title{Return the support of the Bernoulli distribution}
\usage{
\method{support}{Bernoulli}(d, drop = TRUE, ...)
}
\arguments{
\item{d}{An `Bernoulli` object created by a call to [Bernoulli()].}

\item{drop}{logical. Should the result be simplified to a vector if possible?}

\item{...}{Currently not used.}
}
\value{
A vector of length 2 with the minimum and maximum value of the support.
}
\description{
Return the support of the Bernoulli distribution
}
