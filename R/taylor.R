#' @name taylor
#'
#' @title Taylor expansion (one dimension)
#'
#' @description Returns Taylor polynomial approximating a function fn(x)
#'
#' @param fn A function of one variable and that variable must be named 'x'.
#' @param x0 The point in which to to the Taylor expansion.
#' @param ord The order of the Taylor expansion.
#'
#' @return function.
#'
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#'
#' @keywords utilities
#'
#' @examples
#' fn <- function(x) log(x)
#' ord <- 2
#' x0 <- 2
#'
#' xv  <- seq(.2, 5, .1)
#' plot(xv, fn(xv), type="l")
#' lines(xv, taylor(fn, x0=x0, ord=ord)(xv), lty=2)
#' abline(v=x0)
#'
#' fn <- function(x)sin(x)
#' ord <- 4
#' x0 <- 0
#' xv <- seq(-2*pi, 2*pi, 0.1)
#' plot(xv, fn(xv), type="l")
#' lines(xv, taylor(fn, x0=x0, ord=ord)(xv), lty=2)
#' abline(v=x0)
#'
#' 

taylor <- function(fn, x0, ord=1){

    if (!is.function(fn)) stop("'fn' is not a function")

    aname <- names(formals(fn))
    if (length(aname) != 1) stop("'fn' must take a single argument")
    if (!identical(aname, "x")) stop("argument to 'fn' must be called 'x'")
    
    df <- Deriv(fn, nderiv=1:ord)

    quad <- paste0("(x-x0)^")
    aa  <- paste0(quad, 1:ord)

    bb  <- gsub("x0", x0, aa)
    bb
    fac <- factorial(1:ord)
    ff  <- unlist(df(x0)) / fac
    bb  <- paste0(ff, "*", bb)
    bb
    
    out <- function(x, bb){
        tf <- bb
        ps <- lapply(tf, function(s)parse(text=s))
        cc <- lapply(ps, eval, list(x=x))
        dd <- do.call(rbind, cc)
        colSums(dd) + fn(x0)
    }
    
    out2 <- specialize(out, list(bb=bb))
    out2
}
