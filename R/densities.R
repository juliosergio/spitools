###############################
# densities.R
###############################

#' @import data.table
NULL

## dsplinefun() ========================================================================================
#' Wrapper of \code{stats::splinefun} to generate densities and distribution spline interpolation functions
#'
#' \code{dsplinefun} applies \code{\link[stats]{splinefun}} to generate an interpolation function with
#' splines, but it limits the output of the resulting interpolation function to
#' values between 0 and 1 to match the behavior of the density and distribution of probabilities functions.
#'
#' @param ... Any parameters required by \code{\link[stats]{splinefun}} function to specify the spline interpolation.
#'    Typically, this includes the x, y coordinates of the data points.
#'
#' @return returns a function with formal arguments x and deriv, the latter defaulting to zero. This function can be
#'    used to evaluate the interpolating cubic spline (deriv = 0), or its derivatives (deriv = 1, 2, 3) at the
#'    points x, where the spline function interpolates the data points originally specified.
#' @export
dsplinefun <- function (...) {
    f0 <- stats::splinefun(...) # Computes splinefun from stats
    return (
        function (x, deriv=0L) {
            dd <- data.table(x=x, y=f0(x, deriv))
            if (deriv==0L) {
                dd[ y < 0 , y := 0][
                    y > 1 , y := 1]
            }
            return (dd[,y]) # Only Ys are required
        }
    )
}

## dapproxfun() ========================================================================================
#' Wrapper of \code{stats::approxfun} to generate densities linear interpolation functions
#'
#' \code{dapproxfun} applies \code{\link[stats]{approxfun}} to interpolate with splines, but it limits the output
#'    values to be positive to match the behavior of the density of probabilities function.
#'
#' @param ... Any parameters required by \code{\link[stats]{approxfun}} function to specify the interpolation.
#'    Typically, this includes the x, y coordinates of the data points.
#'
#' @return returns a function performing (linear or constant) interpolation of the given data points. For a given
#'    set of x values, this function will return the corresponding interpolated values. It uses data stored in its
#'    environment when it was created.
#' @export
dapproxfun <- function (...) stats::approxfun(..., yleft = 0, yright = 0)


## papproxfun() ========================================================================================
#' Wrapper of \code{stats::approxfun} to generate distribution linear interpolation functions
#'
#' \code{dapproxfun} applies \code{\link[stats]{approxfun}} to interpolate with splines, but it limits the output
#'    values between 0 and 1 to match the behavior of the distribution of probabilities function.
#'
#' @inheritParams dapproxfun
#'
#' @return returns a function performing (linear or constant) interpolation of the given data points. For a given
#'    set of x values, this function will return the corresponding interpolated values. It uses data stored in its
#'    environment when it was created.
#' @export
papproxfun <- function (...) stats::approxfun(..., yleft = 0, yright = 1)


## ffunCreate() ========================================================================================
#' Create density or distribution of probabilities functions from data
#'
#' \code{ffunCreate} creates a density or a distribution of probabilites function from a given set of
#'    data. The resulting function will interpolate the intermediate values using the given \code{aprxf}
#'    approximation function.
#'
#' @param x.data A numeric vector containing the data.
#' @param aprxf A function used to create an interpolation function.
#' @param accumulate If \code{TRUE}, it indicates that the resulting function is a distribution function, i.e.,
#'    an accumulated density function; otherwise, the result will be a density of probabilities
#'    function.
#' @param ... All the other, optional, parameters required by \code{\link[stats]{density}} function to
#'    generate a kernel density estimation from the data.
#'
#' @return Either a denstity or a distribution function from the data. If the parameter \code{accumulate} is
#'    TRUE, the resulting function will be an accumulated density of probabilities function, also known as
#'    a distribution of probabilities function, otherwise, the resulting function will be a density of
#'    probabilities function.
#'
#'    To perform its task, \code{ffunCreate} generates a series of points in the domain and then interpolates
#'    using the interpolation function resulting from applying the given \code{aprxf} approximation function.
#' @seealso
#'    \code{\link[stats]{density}}
#'
#' @examples
#' X <- log(rgamma(150,5)) # Random data generation
#' dfun <- ffunCreate(X)   # To create a density function from X
#' dfun(c(0.45, 1.84, 2.3))
#' plot(dfun, xlim=c(-0.1, 3.0))
#'
#' pfun <- ffunCreate(X, dsplinefun, accumulate=T) # To create a distribution spline interpolated function
#' plot(pfun, xlim=c(-0.1, 3.0))
#'
#' @export
ffunCreate <- function(x.data, aprxf=dapproxfun, accumulate = FALSE, ...) {
    p <- stats::density(x.data, bw = "SJ", ...)
    p <- data.table(x=p$x, yy=p$y)
    if (accumulate) {
        d <- base::mean(p$x-data.table::shift(p$x), na.rm = T)
        p$y <- cumsum(c(0, ((p$yy+data.table::shift(p$yy))/2)[-1]))*d
        p[y > 1, y := 1] # teoricamente la función no puede ser mayor que 1
    } else
        names(p)[2] <- "y"

    aprxf(p$x, p$y)
}


## dSfunCreate() ========================================================================================
#' Create density of probabilities spline interpolated functions from data
#'
#' \code{dSfunCreate} takes a set of data and generates a density of probabilities function usting
#'    splines to interpolate between the resulting sample points.
#'
#' @inheritParams ffunCreate
#' @return returns a function performing spline interpolation on a set of points generated from a kernel
#'    density of probabilities from the given \code{x.data}.
#'
#' @examples
#' X <- log(rgamma(150,5)) # Random data generation
#' dfun <- dSfunCreate(X)   # To create a density function from X
#' dfun(c(0.45, 1.84, 2.3))
#' plot(dfun, xlim=c(-0.1, 3.0))
#'
#' @export
dSfunCreate <- function(x.data, ...) ffunCreate(x.data, dsplinefun, ...)

## pSfunCreate() ========================================================================================
#' Create distribution of probabilities spline interpolated functions from data
#'
#' \code{pSfunCreate} takes a set of data and generates a distribution of probabilities function using
#'    splines to interpolate between the resulting sample points. A distribution of probabilities is
#'    a cumulative distribution function.
#'
#' @inheritParams ffunCreate
#' @return returns a function performing spline interpolation on a set of points generated from a kernel
#'    density of probabilities from the given \code{x.data}. In this case, the resulting kernel desity
#'    points are accumulated prior to the interpolation.
#'
#' @examples
#' X <- log(rgamma(150,5)) # Random data generation
#' pfun <- pSfunCreate(X) # To create a distribution spline interpolated function
#' pfun(c(0.45, 1.84, 2.3))
#' plot(pfun, xlim=c(-0.1, 3.0))
#'
#' @export
pSfunCreate <- function(x.data, ...) ffunCreate(x.data, dsplinefun, accumulate = T, ...)

