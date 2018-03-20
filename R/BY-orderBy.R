#' @title Ordering (sorting) rows of a data frame
#' 
#' @description Ordering (sorting) rows of a data frame by the certain
#'     variables in the data frame. This function is essentially a
#'     wrapper for the \code{order()} function - the important
#'     difference being that variables to order by can be given by a
#'     model formula.
#'
#' @name by-order
#' 
#' @details The sign of the terms in the formula determines whether
#'     sorting should be ascending or decreasing; see examples below
#' 
#' @param formula The right hand side of a formula
#' @param data A dataframe
#' @return The ordered data frame
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk} and Kevin
#'     Wright
#' @seealso \code{\link{transformBy}}, \code{\link{splitBy}}
#' @keywords utilities
#' @examples
#'
#' orderBy(~ conc + Treatment, CO2)
#' ## Sort decreasingly by conc
#' orderBy(~ - conc + Treatment, CO2)

## #' ## Same as:
## #' order_by(CO2, c("conc", "Treatment"))
## #' order_by(CO2, c("-conc", "Treatment"))


#' @rdname by-order
orderBy <- function (formula, data){

    myrank <- function(x){
        rv  <- rep(NA, length(x))
        r   <- rank(cdat[!is.na(cdat)])
        rv[!is.na(cdat)]  <- r
        rv[is.na(rv)]     <- max(r) + 1
        rv
    }


    if (is.vector(formula)) ## E.g. c("Temp", "Month")
        formula <- as.formula(paste0("~ ", paste(formula, collapse=" + ")))

    
    form <- formula
    dat  <- data
    
    if(form[[1]] != "~")
        stop("Error: Formula must be one-sided.")
    
    formc <- as.character(form[2])
    formc <- gsub(" ", "", formc)
    if(!is.element(substring(formc, 1, 1),c("+", "-")))
        formc <- paste("+", formc, sep="")
    
    vars <- unlist(strsplit(formc, "[\\+\\-]"))
    vars <- vars[vars != ""] # Remove spurious "" terms
    
    signs <- formc  
    for (i in 1:length(vars)){
        signs <- gsub(vars[i], "", signs)
    }
    signs <- unlist(strsplit(signs, ""))
    
    orderlist <- list()
    for(i in 1:length(vars)){
        csign <- signs[i]
        cvar  <- vars[i]
        cdat  <- dat[, cvar]
        if (is.factor(cdat)){
            orderlist[[i]] <-
                if (csign == "-") -myrank(cdat) else myrank(cdat) 
        } else {
            orderlist[[i]] <-
                if (csign == "-") -cdat else cdat            
        }
    }
    
    dat[do.call("order",orderlist),,drop=FALSE]
}



## #' @rdname by-order
## order_by <- function(data, formula){

##     if (!(is.data.frame(data) | is.matrix(data)))
##         stop("'data' must be dataframe or matrix")
    
##     formula <- unlist(.rhsf2list(formula))    
##     cls <- class(data)
    
##     if (cls == "matrix")
##         data <- as.data.frame(data)
##     out <- dplyr::arrange_(data, .dots=formula)
##     if (cls == "matrix") as.matrix(out) else out
## }



## orderBy <- function(formula, data){

##     if (!(is.data.frame(data) | is.matrix(data)))
##         stop("'data' must be dataframe or matrix")
    
##     formula <- unlist(.rhsf2list(formula))    
##     cls <- class(data)
    
##     if (cls == "matrix")
##         data <- as.data.frame(data)
##     out <- dplyr::arrange_(data, .dots=formula)
##     if (cls == "matrix") as.matrix(out) else out
## }





