#########################################################################
#'
#' @title Ordering (sorting) rows of a data frame
#' @description Ordering (sorting) rows of a data frame by the certain
#'     variables in the data frame. This function is essentially a
#'     wrapper for the \code{order()} function - the important
#'     difference being that variables to order by can be given by a
#'     model formula.
#' @name by-order
#' 
#########################################################################
#'
#' @details The sign of the terms in the formula determines whether
#'     sorting should be ascending or decreasing; see examples below
#' 
#' @param formula The right hand side of a formula
#' @param data A dataframe
#' @return The ordered data frame
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk} and Kevin
#'     Wright
#' @seealso \code{\link{transformBy}}, \code{\link{transform_by}}, \code{\link{splitBy}}, \code{\link{split_by}}
#' @keywords utilities
#' @examples
#'
#' orderBy(~ conc + Treatment, CO2)
#' ## Sort decreasingly by conc
#' orderBy(~ - conc + Treatment, CO2)

## #' ## Same as:
## #' order_by(CO2, c("conc", "Treatment"))
## #' order_by(CO2, c("-conc", "Treatment"))


#' @export
#' @rdname by-order
order_by <- function(data, formula){
    cl   <- match.call(expand.dots = TRUE)
    cl[[2]] <- formula
    cl[[3]] <- data
    names(cl)[2:3] <- c("formula", "data")
    cl[[1]] <- as.name("orderBy")
    eval(cl)
}

#' @export
#' @rdname by-order
orderBy <- function (formula, data){

    if (!inherits(data, "tbl_df")) is.tib = FALSE
    else {is.tib = TRUE; data = as.data.frame(data)}
    
    myrank <- function(x){
        rv  <- rep(NA, length(x))
        r   <- rank(cout[!is.na(cout)])
        rv[!is.na(cout)]  <- r
        rv[is.na(rv)]     <- max(r) + 1
        rv
    }


    if (is.vector(formula)) ## E.g. c("Temp", "Month")
        formula <- as.formula(paste0("~ ", paste(formula, collapse=" + ")))
    
    form <- formula
    out  <- data

    
    
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
        cout  <- out[, cvar]
        if (is.factor(cout)){
            orderlist[[i]] <-
                if (csign == "-") -myrank(cout) else myrank(cout) 
        } else {
            orderlist[[i]] <-
                if (csign == "-") -cout else cout            
        }
    }
    
    out <- out[do.call("order", orderlist),, drop=FALSE]

    if (is.tib) as_tibble(out) else out
    ##out
}











