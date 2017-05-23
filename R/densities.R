###############################
# densities.R
###############################


#' Creation of approximate kernel density estimation function.
#'
#' \code{dfunCreate} takes a series of data and returns a continuous function that approximates
#' a kernel density estimation, for such data.
#'
#' This function adjusts \code{\link{stats::density}} results to a linearly interpolated function
dfunCreate <- (function(...) stats::approxfun(..., yleft = 0, yright = 0)) %cmp%
    (function(x, ...) stats::density(x, bw = "SJ", ...))
# USO:
#    dfun <- dfunCreate(X) # donde X es el vector de datos
#      y luego se puede usar esta función
#      p.ej.
#
#    X <- log(rgamma(150,5)) # Creación aleatoria de datos
#    dfun <- dfunCreate(X)
#    dfun(c(0.45, 1.84, 2.3))
#      o para graficarla:
#    plot(dfun, xlim=c(-0.1, 3.0))

pfunCreate <- function(x.data) {
    p <- stats::density(x.data, bw = "SJ")
    d <- base::mean(p$x-stats::lag(p$x), na.rm = T)
    # p$ny <- cumsum(p$y)*d
    # Integración trapecial
    p$ny <- base::cumsum(c(0, ((p$y+stats::lag(p$y))/2)[-1]))*d
    p$ny[p$ny > 1] <- 1 # teoricamente la función no puede ser mayor que 1
    stats::approxfun(p$x, p$ny, yleft = 0, yright = 1)
}
