% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Exponential.R
\name{Exponential}
\alias{Exponential}
\title{Create an Exponential distribution}
\usage{
Exponential(rate = 1)
}
\arguments{
\item{rate}{The rate parameter, written \eqn{\lambda} in textbooks.
Can be any positive number. Defaults to `1`.}
}
\value{
An `Exponential` object.
}
\description{
Exponential distributions are frequently used for modeling the amount
of time that passes until a specific event occurs. For example, exponential
distributions could be used to model the time between two earthquakes,
the amount of delay between internet packets, or the amount of time a piece
of machinery can run before needing repair.
}
\details{
We recommend reading this documentation on
  <https://alexpghayes.github.io/distributions3/>, where the math
  will render with additional detail and much greater clarity.

  In the following, let \eqn{X} be an Exponential random variable with
  rate parameter `rate` = \eqn{\lambda}.

  **Support**: {x in [0, \eqn{\infty})}

  **Mean**: 1 / \eqn{\lambda}

  **Variance**: 1 / \eqn{\lambda^2}

  **Probability density function (p.d.f)**:

  \deqn{
    f(x) = \lambda e^{-\lambda x}
  }

  **Cumulative distribution function (c.d.f)**:

  \deqn{
    F(x) = 1 - e^{-\lambda x}
  }

  **Moment generating function (m.g.f)**:

  \deqn{
    \frac{\lambda}{\lambda - t}, for t < \lambda
  }
}
\examples{

set.seed(27)

X <- Exponential(5)
X

mean(X)
variance(X)
skewness(X)
kurtosis(X)

random(X, 10)

pdf(X, 2)
log_pdf(X, 2)

cdf(X, 4)
quantile(X, 0.7)

cdf(X, quantile(X, 0.7))
quantile(X, cdf(X, 7))
}
\seealso{
Other continuous distributions: 
\code{\link{Beta}()},
\code{\link{Cauchy}()},
\code{\link{ChiSquare}()},
\code{\link{Erlang}()},
\code{\link{FisherF}()},
\code{\link{Frechet}()},
\code{\link{GEV}()},
\code{\link{GP}()},
\code{\link{Gamma}()},
\code{\link{Gumbel}()},
\code{\link{LogNormal}()},
\code{\link{Logistic}()},
\code{\link{Normal}()},
\code{\link{RevWeibull}()},
\code{\link{StudentsT}()},
\code{\link{Tukey}()},
\code{\link{Uniform}()},
\code{\link{Weibull}()}
}
\concept{continuous distributions}
