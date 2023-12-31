% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sd.R
\name{sd}
\alias{sd}
\title{Standard Deviation, Including Rvecs}
\usage{
sd(x, na.rm = FALSE)
}
\arguments{
\item{x}{A numeric vector or R object, including an \code{\link[=rvec]{rvec()}}.}

\item{na.rm}{Whether to remove \code{NA}s before calculating standard deviations.}
}
\value{
An rvec, if \code{x} is an rvec. Otherwise
typically a numeric vector.
}
\description{
Calculate standard deviation of \code{x}, where \code{x} can be
an rvec. If \code{x} is an rvec, separate standard deviations
are calculated for each draw.
}
\details{
To enable different behavior for rvecs and for ordinary vectors,
the base R function \code{\link[stats:sd]{stats::sd()}} is turned into a generic,
with \code{\link[stats:sd]{stats::sd()}} as the default.

For details on the calculations, see the documentation
for \code{\link[stats:sd]{stats::sd()}}.
}
\examples{
x <- rvec(cbind(rnorm(10), rnorm(10, sd = 20)))
x
sd(x)
}
\seealso{
\code{\link[=var]{var()}}
}
