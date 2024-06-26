% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HyperGeometric.R
\name{support.HyperGeometric}
\alias{support.HyperGeometric}
\title{Return the support of the HyperGeometric distribution}
\usage{
\method{support}{HyperGeometric}(d, drop = TRUE, ...)
}
\arguments{
\item{d}{An `HyperGeometric` object created by a call to [HyperGeometric()].}

\item{drop}{logical. Should the result be simplified to a vector if possible?}

\item{...}{Currently not used.}
}
\value{
A vector of length 2 with the minimum and maximum value of the support.
}
\description{
Return the support of the HyperGeometric distribution
}
