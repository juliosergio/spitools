##########################
# utilities.R
##########################

# Function composition
composite <- function(f,g) function(...) f(g(...))
# as operator:
# `%cmp%` <- composite
`%cmp%` <- function(f,g) function(...) f(g(...))

`%,%` <- function(x, y) paste0(x, y) # string concatenate

`%//%` <- function(x,y) as.integer(x/y) # Integer division

ftst <- function(x) print(x)

group.mean <- function(x, ini=1, size=12) {
    # Computes the mean of groups inside 'x', each of size 'size'
    # It can be used to compute accumulated anual means
    # The groups in 'x' start with element 'ini'
    # ------------

    # elements to be taken into account
    n <- length(x) - ini + 1
    m <- floor(n/size) # complete groups
    fin <- ini + m*size -1
    sum(x[ini:fin])/m
}

# Statistical Mode functions

id.mode <- function(tt) as.numeric(names(tt)[which.max(tt)])  # mode from a frequency table
# The mode of a data series is the composition of table(), which computes frequencies in the series,
# and id.mode()
stat.mode <- id.mode %cmp% base::table # stat.mode(x) where x is the data series
# diference on mode
get.dif <- function(x) stat.mode(x-stats::lag(x))

dist2 <- function(pts, p0, pw=1) {
    # Calcula la distancia del punto p0 a todos los puntos en el conjunto pts
    # Computes distance from p0 to each point in set 'pts'
    # 'pw' is the power of the sum of squares
    r <- (pts[,1]-p0[,1])^2 + (pts[,2]-p0[,2])^2
    if (pw==2) r else r^(pw/2)
}

mdist2 <- function(pts, mp0, pw=1) {
    # The same as 'dist0', but now mp0 is also a set of points
    # so, the result will be a matrix, where each column will contain
    # the distances from each element of 'pts', to the corresponding
    # point in 'mp0'.
    sapply(as.data.frame(t(mp0)), function(p) dist2(pts, t(p), pw))
}

# Comparison that includes NAs
compareNA <- function(v1,v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
}
# as operator
`%=%`<- compareNA

# One arg. function producer
ffun <- function(f, ...) function(x) f(x, ...)
# e.g. a function that always adds 2:
# f0 <- ffun(`+`, 2)
# f0(5) ===> 7

# Sintaxis de ifelse multiple -------------------------------------------------------
# library(lazyeval)
i_ <- function(if_stat, then) {
    if_stat <- lazyeval::expr_text(if_stat)
    then    <- lazyeval::expr_text(then)
    sprintf("ifelse(%s, %s, ", if_stat, then)
}

e_ <- function(else_ret) {
    else_ret <- lazyeval::expr_text(else_ret)
    else_ret
}

if.else_ <- function(...) {
    args <- list(...)

    for (i in 1:(length(args) - 1) ) {
        if (substr(args[[i]], 1, 6) != "ifelse") {
            stop("All but the last argument, need to be if.then_ functions.", call. = FALSE)
        }
    }
    if (substr(args[[length(args)]], 1, 6) == "ifelse"){
        stop("Last argument needs to be an else_ function.", call. = FALSE)
    }
    args$final <- paste(rep(')', length(args) - 1), collapse = '')
    eval_string <- do.call('paste', args)
    eval(parse(text = eval_string), envir = parent.frame())
}

test.ie <- function() {
    dd <- data.frame(a=c(1,2,1,3), b=1:4)
    if.else_(
        i_(dd$a==1, dd$b),
        i_(dd$a==2, dd$b*100),
        e_(-dd$b)
    ) # GIVES: c(1, 200, 3, -4)
}

# Lo mismo que lo anterior pero con la funcionalidad de data.table

test.dt <- function() {
    dd <- data.frame(a=c(1L,2L,1L,3L), b=1:4)
    setDT(dd)[
                , c := -b    ][ # Default
        a == 1L , c := b     ][
        a == 2L , c := 100*b ][,c] # <- GIVES only vector or, all dd -> []
}

# END Sintaxis de ifelse multiple -------------------------------------------------------




