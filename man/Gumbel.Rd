% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gumbel.R
\name{Gumbel}
\alias{Gumbel}
\title{Create a Gumbel distribution}
\usage{
Gumbel(mu = 0, sigma = 1)
}
\arguments{
\item{mu}{The location parameter, written \eqn{\mu} in textbooks.
`mu` can be any real number.  Defaults to `0`.}

\item{sigma}{The scale parameter, written \eqn{\sigma} in textbooks.
`sigma` can be any positive number.  Defaults to `1`.}
}
\value{
A `Gumbel` object.
}
\description{
The Gumbel distribution is a special case of the `\link{GEV}` distribution,
obtained when the GEV shape parameter \eqn{\xi} is equal to 0.
It may be referred to as a type I extreme value distribution.
}
\details{
We recommend reading this documentation on
  <https://alexpghayes.github.io/distributions3/>, where the math
  will render with additional detail and much greater clarity.

  In the following, let \eqn{X} be a Gumbel random variable with location
  parameter  `mu` = \eqn{\mu}, scale parameter `sigma` = \eqn{\sigma}.

  **Support**: \eqn{R}, the set of all real numbers.

  **Mean**: \eqn{\mu + \sigma\gamma}, where \eqn{\gamma} is Euler's
  constant, approximately equal to 0.57722.

  **Median**: \eqn{\mu - \sigma\ln(\ln 2)}{\mu - \sigma ln(ln 2)}.

  **Variance**: \eqn{\sigma^2 \pi^2 / 6}.

  **Probability density function (p.d.f)**:

  \deqn{f(x) = \sigma ^ {-1} \exp[-(x - \mu) / \sigma]%
        \exp\{-\exp[-(x - \mu) / \sigma] \}}{%
       f(x) = (1 / \sigma) exp[-(x - \mu) / \sigma]%
        exp{-exp[-(x - \mu) / \sigma]}}
  for \eqn{x} in \eqn{R}, the set of all real numbers.

  **Cumulative distribution function (c.d.f)**:

  In the \eqn{\xi = 0} (Gumbel) special case
  \deqn{F(x) = \exp\{-\exp[-(x - \mu) / \sigma] \}}{%
        F(x) = exp{ - exp[-(x - \mu) / \sigma]} }
  for \eqn{x} in \eqn{R}, the set of all real numbers.
}
\examples{

set.seed(27)

X <- Gumbel(1, 2)
X

random(X, 10)

pdf(X, 0.7)
log_pdf(X, 0.7)

cdf(X, 0.7)
quantile(X, 0.7)

cdf(X, quantile(X, 0.7))
quantile(X, cdf(X, 0.7))
}
\seealso{
Other continuous distributions: 
\code{\link{Beta}()},
\code{\link{Cauchy}()},
\code{\link{ChiSquare}()},
\code{\link{Erlang}()},
\code{\link{Exponential}()},
\code{\link{FisherF}()},
\code{\link{Frechet}()},
\code{\link{GEV}()},
\code{\link{GP}()},
\code{\link{Gamma}()},
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
