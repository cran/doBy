#' @title Extract components from a formula with "conditioning bar"
#' 
#' @description Extract components from a formula with the form
#'     \code{y ~ x1 + ... + xn | g1 + ... + gm}
#' 
#' @param form A formula of the form \code{y ~ x1 + ... + xn | g1 + ... + gm}
#' @return If the formula is \code{y ~ x1 + x2 | g1 + g2} the result is
#'     \item{model}{\code{y ~ x1 + x2}} \item{groups}{\code{ g1 + g2}}
#'     \item{groupFormula}{\code{~ g1 + g2}}
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @keywords utilities
#' @examples
#' 
#' gf <- parseGroupFormula(y ~ x1 + x2 | g1 + g2)
#' gf 
#' 
#' @export parseGroupFormula
parseGroupFormula <- function(form)
{

    if (!inherits(form, "formula") || length(form) != 3)
        stop("formula must be a two-sided formula object")
    rhs <- form[[3]]

    if (!inherits(rhs, "call") || rhs[[1]] != as.symbol('|'))
        stop("rhs of formula must be a conditioning expression")
    form[[3]] <- rhs[[2]]
    groups <- rhs[[3]]
    grpFormula <- as.formula(paste("~", deparse(groups)))
    list(model = form, groups = groups, groupFormula=grpFormula)
}

#' @title Convert right hand sided formula to a list
#' @description Convert right hand sided formula to a list
#' @param f A right hand sided formula

.rhsf2list <- function (f) {
    if (is.character(f))    list(f) 
    else if (is.numeric(f)) lapply(list(f), "as.character")
    else if (is.list(f))    lapply(f, "as.character")
    else {
        .xxx. <- f[[length(f)]]
        f1 <- unlist(strsplit(paste(deparse(.xxx.), collapse = ""), 
                              " *\\+ *"))
        f2 <- unlist(lapply(f1, strsplit, " *\\* *| *: *| *\\| *"), 
                     recursive = FALSE)
        f2
    }
}


#' Where are the n largest or n smallest elements in a numeric vector ?
#' 
#' Determines the locations, i.e., indices of the n largest or n smallest
#' elements of a numeric vector.

#' 
#' @aliases which.maxn which.minn
#' @param x numeric vector
#' @param n integer >= 1
#' @return A vector of length at most n with the indices of the n largest /
#'     smaller elements. NAs are discarded and that can cause the vector to be
#'     smaller than n.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{which.max}}, \code{\link{which.min}}
#' @keywords utilities
#' @examples
#' 
#' x <- c(1:4, 0:5, 11, NA, NA)
#' ii <- which.minn(x, 5)
#' 
#' x <- c(1, rep(NA,10), 2)
#' ii <- which.minn(x, 5)
#' 
#' @export which.maxn
which.maxn <- function(x, n = 1) {
  if (n==1)
    which.max(x)
  else
    {
      if (n > 1){
        ii <- order(x,decreasing = TRUE)[1:min(n, length(x))]
        ii[!is.na(x[ii])]
      }
      else {
       stop("n must be >= 1")
      }
    }
}

#' @export
which.minn <- function(x, n = 1) {
  if (n == 1)
    which.min(x)
  else
    {
      if (n > 1) {
        ii <- order(x,decreasing=FALSE)[1:min(n, length(x))]
        ii[!is.na(x[ii])]
      }
      else {
       stop("n must be >= 1")
      }
    }
}




#' Truncate values in a matrix / vector to zero if they are below a certain threshold.
#' 
#' @param x matrix / vector
#' @param tol threshold
#' @param sparse logical; if TRUE and `x` is a matrix, return a sparse matrix
#' 
#' @export
truncate0 <- function(x, tol=0.6, sparse=TRUE){ ## doBy candidate
    x[abs(x) <= tol] <- 0
    if (is.matrix(x)){
      if (sparse) as(x, "dgCMatrix") else x
    }
}



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
#'

#' @export
taylor <- function(fn, x0, ord=1){

    if (!is.function(fn))
        stop("'fn' is not a function")

    aname <- names(formals(fn))
    if (length(aname) != 1)
        stop("'fn' must take a single argument")
    if (!identical(aname, "x"))
        stop("argument to 'fn' must be called 'x'")
    
    df <- Deriv(fn, nderiv=1:ord)

    quad <- paste0("(x-x0)^")
    aa  <- paste0(quad, 1:ord)

    bb  <- gsub("x0", x0, aa)
    bb
    fac <- factorial(1:ord)
    ff  <- unlist(df(x0)) / fac
    bb  <- paste0(ff, "*", bb)
    bb

    expr <- parse(text=(paste0(bb, collapse="+")))
    fun <- expr_to_fun(expr)

    fun
    
}



#' Recode values of a vector
#' 
#' Recodes a vector with values, say 1,2 to a variable with values, say 'a',
#' 'b'
#' 
#' @param x A vector; the variable to be recoded.
#' @param src The source values: a subset of the present values of x
#' @param tgt The target values: the corresponding new values of x
#' @param default Default target value for those values of x not listed in
#'     `src`. When default=NULL, values of x which are not given in `src` will
#'     be kept in the output.
#' @param keep.na If TRUE then NA's in x will be retained in the output
#' @return A vector
#' @section Warning : Care should be taken if x is a factor. A safe approach may
#'     be to convert x to a character vector using as.character.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link[base]{cut}}, \code{\link[base]{factor}},
#'     \code{\link[doBy]{recodeVar}}
#' @keywords utilities
#' @examples
#' 
#' x <- c("dec", "jan", "feb", "mar", "apr", "may")
#' src1 <- list(c("dec", "jan", "feb"), c("mar", "apr", "may"))
#' tgt1 <- list("winter", "spring")
#' recodeVar(x, src=src1, tgt=tgt1)
#' #[1] "winter" "winter" "winter" "spring" "spring" "spring"
#' 
#' x <- c(rep(1:3, 3))
#' #[1] 1 2 3 1 2 3 1 2 3
#' 
#' ## Simple usage:
#' recodeVar(x, src=c(1, 2), tgt=c("A", "B"))
#' #[1] "A" "B" NA  "A" "B" NA  "A" "B" NA 
#' 
#' ## Here we need to use lists
#' recodeVar(x, src=list(c(1, 2)), tgt=list("A"))
#' #[1] "A" "A" NA  "A" "A" NA  "A" "A" NA 
#' recodeVar(x, src=list(c(1, 2)), tgt=list("A"), default="L")
#' #[1] "A" "A" "L" "A" "A" "L" "A" "A" "L"
#' recodeVar(x, src=list(c(1, 2), 3), tgt=list("A", "B"), default="L")
#' #[1] "A" "A" "B" "A" "A" "B" "A" "A" "B"
#' 
#' ## Dealing with NA's in x
#' x<-c(NA,rep(1:3, 3),NA)
#' #[1] NA  1  2  3  1  2  3  1  2  3 NA
#' recodeVar(x, src=list(c(1, 2)), tgt=list("A"))
#' #[1] NA  "A" "A" NA  "A" "A" NA  "A" "A" NA  NA 
#' recodeVar(x, src=list(c(1, 2)), tgt=list("A"), default="L")
#' #[1] NA  "A" "A" "L" "A" "A" "L" "A" "A" "L" NA 
#' recodeVar(x, src=list(c(1, 2)), tgt=list("A"), default="L", keep.na=FALSE)
#' #[1] "L" "A" "A" "L" "A" "A" "L" "A" "A" "L" "L"
#' 
#' x <- c("no", "yes", "not registered", "no", "yes", "no answer")
#' recodeVar(x, src = c("no", "yes"), tgt = c("0", "1"), default = NA)
#' 
#' 
#' @export recodeVar
recodeVar <- function(x, src, tgt, default=NULL, keep.na=TRUE) {

  if (length(src)!=length(tgt)) {
    stop("length of src not equal to length of tgt")
  }
    mtc <- lapply(src,
                  function(zzz) {
                      which(x %in% zzz)
                  })
    
  idx <- seq_along(x)
  unmatch <- setdiff(idx, unlist(mtc))
  
  if (is.factor(x)) {
    val <- as.character(x)
  } else {
    val <- x
  }
  for (ii in 1:length(tgt))
    val[mtc[[ii]]] <- tgt[[ii]]

  if (!is.null(default)) {
    if (keep.na) {
      iii <- intersect(which(!is.na(x)), unmatch)
      val[iii] <- default
    } else {
      val[unmatch] <- default
    }
  }
  
  if (is.factor(x))
    val <- as.factor(val)
  val

  return(val)
}

#' @rdname recodeVar
#' @export
recode_var <- recodeVar


#' Rename columns in a matrix or a dataframe.
#' 
#' Rename columns in a matrix or a dataframe.
#' 
#' 
#' @param indata A dataframe or a matrix
#' @param src Source: Vector of names of columns in `indata` to be
#'     renamed. Can also be a vector of column numbers.
#' @param tgt Target: Vector with corresponding new names in the
#'     output.
#' @return A dataframe if `indata` is a dataframe; a matrix in
#'     `indata` is a matrix.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @keywords utitlities
#' @examples
#' renameCol(CO2, 1:2, c("kk", "ll"))
#' renameCol(CO2, c("Plant", "Type"), c("kk", "ll"))
#' 
#' # These fail - as they should:
#' # renameCol(CO2, c("Plant", "Type", "conc"), c("kk", "ll"))
#' # renameCol(CO2, c("Plant", "Type", "Plant"), c("kk", "ll"))
#' 
#' @export renameCol
renameCol <- function(indata, src, tgt){

  if (inherits(indata, "data.frame")) {
    isDF <- TRUE
    dfnames <- names(indata)
  } else {
    if (inherits(indata, "matrix")) {
      isDF <- FALSE
      dfnames <- colnames(indata)
    } else {
      stop("'indata' must be either a dataframe or a matrix")
    }
  }
  
  if (length(src)!=length(unique(src))){
    stop("A src name has been repeated")
  }

  if (length(tgt)!=length(unique(tgt))){
    stop("A tgt name has been repeated")
  }

  if (length(src)!=length(tgt)){
    stop("length of src not equal to length of tgt")
  }
    
  if (is.numeric(src)){
    idx <- src
    iii <- intersect(seq_along(dfnames), src)
    iii <- setdiff(src, iii)
    if (length(iii)>0){
      sss <- sprintf("Column(s) %s are not in 'indata'", toString(iii))
      stop(sss)
    }
  } else {
    idx <- match(src, dfnames)
    if (any(is.na(idx))){
      sss <- sprintf("Column names %s are not in 'indata'", toString(src[is.na(idx)]))
      stop(sss)
    }
  }
  
  ans <- indata
  if (isDF){
    names(ans)[idx] <- tgt
  } else {
    colnames(ans)[idx] <- tgt
  }

  return(ans)
}



###############################################################################
#' @title Computing simple descriptive statistics of a numeric vector.
#' @description Computing simple descriptive statistics of a numeric
#'     vector - not unlike what proc means of SAS does
###############################################################################
#' @param x A numeric vector
#' @param na.rm Should missing values be removed
#' @return A vector with named elements.
#' @author Gregor Gorjanc; \email{gregor.gorjanc@@bf.uni-lj.si}
#' @seealso \code{\link{summaryBy}}, \code{\link{summary_by}}
#' @keywords utilities
#' @examples
#' 
#' x <- c(1, 2, 3, 4, NA, NaN)
#' descStat(x)
#' 
#' @export descStat
descStat <- function (x, na.rm = TRUE)
{
  if(!is.numeric(x))
    stop("'x' must be numeric")
  m <- mean(x, na.rm = na.rm)
  s <- sd(x, na.rm = na.rm)
  c(n = length(x),
    obs = sum(!is.na(x)),
    mean = m,
    median = median(x, na.rm = na.rm),
    sd = s,
    cv = s/m,
    min = min(x, na.rm = na.rm),
    max = max(x, na.rm = na.rm))
}

