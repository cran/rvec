% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distributions.R
\name{dexp_rvec}
\alias{dexp_rvec}
\alias{pexp_rvec}
\alias{qexp_rvec}
\alias{rexp_rvec}
\title{The Exponential Distribution, Using Multiple Draws}
\usage{
dexp_rvec(x, rate = 1, log = FALSE)

pexp_rvec(q, rate = 1, lower.tail = TRUE, log.p = FALSE)

qexp_rvec(p, rate = 1, lower.tail = TRUE, log.p = FALSE)

rexp_rvec(n, rate = 1, n_draw = NULL)
}
\arguments{
\item{x}{Quantiles. Can be an rvec.}

\item{rate}{Vector of rates.
See \code{\link[stats:Exponential]{stats::dexp()}}.
Can be an rvec.}

\item{log, log.p}{Whether to return results
on a log scale. Default is
\code{FALSE}. Cannot be an rvec.}

\item{q}{Quantiles. Can be an rvec.}

\item{lower.tail}{Whether to return
\eqn{P[X \le x]}, as opposed to
\eqn{P[X > x]}. Default is \code{TRUE}.
Cannot be an rvec.}

\item{p}{Probabilities. Can be an rvec.}

\item{n}{The length of random vector being
created. Cannot be an rvec.}

\item{n_draw}{Number of random draws
in the random vector being
created. Cannot be an rvec.}
}
\value{
\itemize{
\item If any of the arguments are rvecs,
or if a value for \code{n_draw} is supplied,
then an \link[=rvec]{rvec}
\item Otherwise an ordinary R vector.
}
}
\description{
Density, distribution function,
quantile function and random generation for the
exponential distribution, modified to work with
rvecs.
}
\details{
Functions \code{dexp_rvec()}, \code{pexp_rvec()},
\code{pexp_rvec()} and \code{rexp_rvec()} work like
base R functions \code{\link[=dexp]{dexp()}}, \code{\link[=pexp]{pexp()}},
\code{\link[=qexp]{qexp()}}, and \code{\link[=rexp]{rexp()}}, except that
they accept rvecs as inputs. If any
input is an rvec, then the output will be too.
Function \code{rexp_rvec()} also returns an
rvec if a value for \code{n_draw} is supplied.

\code{dexp_rvec()}, \code{pexp_rvec()},
\code{pexp_rvec()} and \code{rexp_rvec()}
use \link[vctrs:theory-faq-recycling]{tidyverse}
vector recycling rules:
\itemize{
\item Vectors of length 1 are recycled
\item All other vectors must have the same size
}
}
\examples{
x <- rvec(list(c(3, 5.1),
               c(0.1, 2.3)))
dexp_rvec(x, rate = 1.5)
pexp_rvec(x, rate = 1.5)

rexp_rvec(n = 2,
          rate = c(1.5, 4),
          n_draw = 1000)
}
\seealso{
\itemize{
\item \code{\link[=dexp]{dexp()}}
\item \code{\link[=pexp]{pexp()}}
\item \code{\link[=qexp]{qexp()}}
\item \code{\link[=rexp]{rexp()}}
\item \link[stats:Distributions]{stats::distributions}.
}
}
