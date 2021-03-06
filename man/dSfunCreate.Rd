% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/densities.R
\name{dSfunCreate}
\alias{dSfunCreate}
\title{Create density of probabilities spline interpolated functions from data}
\usage{
dSfunCreate(x.data, ...)
}
\arguments{
\item{x.data}{A numeric vector containing the data.}

\item{...}{All the other, optional, parameters required by \code{\link[stats]{density}} function to
generate a kernel density estimation from the data.}
}
\value{
returns a function performing spline interpolation on a set of points generated from a kernel
   density of probabilities from the given \code{x.data}.
}
\description{
\code{dSfunCreate} takes a set of data and generates a density of probabilities function usting
   splines to interpolate between the resulting sample points.
}
\examples{
X <- log(rgamma(150,5)) # Random data generation
dfun <- dSfunCreate(X)   # To create a density function from X
dfun(c(0.45, 1.84, 2.3))
plot(dfun, xlim=c(-0.1, 3.0))

}
\seealso{
\code{\link{ffunCreate}} for a more generic function.
}
