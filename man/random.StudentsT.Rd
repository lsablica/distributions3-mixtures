% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StudentsT.R
\name{random.StudentsT}
\alias{random.StudentsT}
\title{Draw a random sample from a StudentsT distribution}
\usage{
\method{random}{StudentsT}(x, n = 1L, drop = TRUE, ...)
}
\arguments{
\item{x}{A `StudentsT` object created by a call to [StudentsT()].}

\item{n}{The number of samples to draw. Defaults to `1L`.}

\item{drop}{logical. Should the result be simplified to a vector if possible?}

\item{...}{Unused. Unevaluated arguments will generate a warning to
catch mispellings or other possible errors.}
}
\value{
In case of a single distribution object or `n = 1`, either a numeric
  vector of length `n` (if `drop = TRUE`, default) or a `matrix` with `n` columns
  (if `drop = FALSE`).
}
\description{
Please see the documentation of [StudentsT()] for some properties
of the T distribution, as well as extensive examples
showing to how calculate p-values and confidence intervals.
}
\examples{

set.seed(27)

X <- StudentsT(3)
X

random(X, 10)

pdf(X, 2)
log_pdf(X, 2)

cdf(X, 4)
quantile(X, 0.7)

### example: calculating p-values for two-sided T-test

# here the null hypothesis is H_0: mu = 3

# data to test
x <- c(3, 7, 11, 0, 7, 0, 4, 5, 6, 2)
nx <- length(x)

# calculate the T-statistic
t_stat <- (mean(x) - 3) / (sd(x) / sqrt(nx))
t_stat

# null distribution of statistic depends on sample size!
T <- StudentsT(df = nx - 1)

# calculate the two-sided p-value
1 - cdf(T, abs(t_stat)) + cdf(T, -abs(t_stat))

# exactly equivalent to the above
2 * cdf(T, -abs(t_stat))

# p-value for one-sided test
# H_0: mu <= 3   vs   H_A: mu > 3
1 - cdf(T, t_stat)

# p-value for one-sided test
# H_0: mu >= 3   vs   H_A: mu < 3
cdf(T, t_stat)

### example: calculating a 88 percent T CI for a mean

# lower-bound
mean(x) - quantile(T, 1 - 0.12 / 2) * sd(x) / sqrt(nx)

# upper-bound
mean(x) + quantile(T, 1 - 0.12 / 2) * sd(x) / sqrt(nx)

# equivalent to
mean(x) + c(-1, 1) * quantile(T, 1 - 0.12 / 2) * sd(x) / sqrt(nx)

# also equivalent to
mean(x) + quantile(T, 0.12 / 2) * sd(x) / sqrt(nx)
mean(x) + quantile(T, 1 - 0.12 / 2) * sd(x) / sqrt(nx)
}
\seealso{
Other StudentsT distribution: 
\code{\link{cdf.StudentsT}()},
\code{\link{pdf.StudentsT}()},
\code{\link{quantile.StudentsT}()}
}
\concept{StudentsT distribution}
