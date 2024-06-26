% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Categorical.R
\name{pdf.Categorical}
\alias{pdf.Categorical}
\alias{log_pdf.Categorical}
\title{Evaluate the probability mass function of a Categorical discrete distribution}
\usage{
\method{pdf}{Categorical}(d, x, ...)

\method{log_pdf}{Categorical}(d, x, ...)
}
\arguments{
\item{d}{A `Categorical` object created by a call to [Categorical()].}

\item{x}{A vector of elements whose probabilities you would like to
determine given the distribution `d`.}

\item{...}{Unused. Unevaluated arguments will generate a warning to
catch mispellings or other possible errors.}
}
\value{
A vector of probabilities, one for each element of `x`.
}
\description{
Evaluate the probability mass function of a Categorical discrete distribution
}
\examples{

set.seed(27)

X <- Categorical(1:3, p = c(0.4, 0.1, 0.5))
X

Y <- Categorical(LETTERS[1:4])
Y

random(X, 10)
random(Y, 10)

pdf(X, 1)
log_pdf(X, 1)

cdf(X, 1)
quantile(X, 0.5)

# cdfs are only defined for numeric sample spaces. this errors!
# cdf(Y, "a")

# same for quantiles. this also errors!
# quantile(Y, 0.7)
}
