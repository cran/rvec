% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distributions.R
\name{dlnorm_rvec}
\alias{dlnorm_rvec}
\alias{plnorm_rvec}
\alias{qlnorm_rvec}
\alias{rlnorm_rvec}
\title{The Log-Normal Distribution, Using Multiple Draws}
\usage{
dlnorm_rvec(x, meanlog = 0, sdlog = 1, log = FALSE)

plnorm_rvec(q, meanlog = 0, sdlog = 1, lower.tail = TRUE, log.p = FALSE)

qlnorm_rvec(p, meanlog = 0, sdlog = 1, lower.tail = TRUE, log.p = FALSE)

rlnorm_rvec(n, meanlog = 0, sdlog = 1, n_draw = NULL)
}
\arguments{
\item{x}{Quantiles. Can be an rvec.}

\item{meanlog}{Mean of distribution, on log scale.
Default is \code{0}. See \code{\link[stats:Lognormal]{stats::dlnorm()}}.
Can be an rvec.}

\item{sdlog}{Standard deviation of distribution,
on log scale. Default is \code{1}. See \code{\link[stats:Lognormal]{stats::dlnorm()}}.
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
log-normal distribution, modified to work with
rvecs.
}
\details{
Functions \code{dlnorm_rvec()}, \code{plnorm_rvec()},
\code{plnorm_rvec()} and \code{rlnorm_rvec()} work like
base R functions \code{\link[=dlnorm]{dlnorm()}}, \code{\link[=plnorm]{plnorm()}},
\code{\link[=qlnorm]{qlnorm()}}, and \code{\link[=rlnorm]{rlnorm()}}, except that
they accept rvecs as inputs. If any
input is an rvec, then the output will be too.
Function \code{rlnorm_rvec()} also returns an
rvec if a value for \code{n_draw} is supplied.

\code{dlnorm_rvec()}, \code{plnorm_rvec()},
\code{plnorm_rvec()} and \code{rlnorm_rvec()}
use \link[vctrs:theory-faq-recycling]{tidyverse}
vector recycling rules:
\itemize{
\item Vectors of length 1 are recycled
\item All other vectors must have the same size
}
}
\examples{
x <- rvec(list(c(3.1, 5.7),
               c(0.2, 2.3)))
dlnorm_rvec(x)
plnorm_rvec(x)

rlnorm_rvec(n = 2,
            meanlog = c(1, 3),
            n_draw = 1000)
}
\seealso{
\itemize{
\item \code{\link[=dlnorm]{dlnorm()}}
\item \code{\link[=plnorm]{plnorm()}}
\item \code{\link[=qlnorm]{qlnorm()}}
\item \code{\link[=rlnorm]{rlnorm()}}
\item \link[stats:Distributions]{stats::distributions}.
}
}
