% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docs-datasets.R
\docType{data}
\name{reg_post}
\alias{reg_post}
\title{Posterior Sample from Linear Regression}
\format{
A matrix with 200 columns and
the following rows:
\itemize{
\item \code{alpha}: Intercept parameter
\item \code{beta}: Slope parameter
\item \code{sigma}: Standard deviation of error term
}
}
\source{
\code{reg_post} contains values from the second
half of the \code{line} dataset
in package \href{https://CRAN.R-project.org/package=coda}{coda}.
The line dataset draws on the BUGS manual:
Spiegelhalter, D.J., Thomas, A., Best, N.G. and
Gilks, W.R. (1995) BUGS: Bayesian inference using
Gibbs Sampling, Version 0.5, MRC Biostatistics Unit,
Cambridge.
}
\usage{
reg_post
}
\description{
Posterior sample for parameters from a linear
regression model.
}
\keyword{datasets}
