#######################################################################
#'
#' @title Formula based version of lapply and sapply
#' @description This function is a wrapper for calling lapply on the
#'     list resulting from first calling splitBy.
#' @name by-lapply
#' 
#######################################################################
#' @param formula A formula describing how data should be split.
#' @param data A dataframe.
#' @param FUN A function to be applied to each element in the splitted
#'    list, see 'Examples' below.
#' @param ... optional arguments to FUN.
#' @param USE.NAMES Same as for `sapply`
#' @param simplify Same as for `sapply`
#'
#' @return A list.
#'
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#'
#' @seealso \code{\link{splitBy}}, \code{\link{split_by}}
#'
#' @keywords utilities
#'
#' @examples
#' fun <- function(x) range(x$uptake)
#' lapplyBy(~Treatment + Type, data=CO2, FUN=fun)
#' sapplyBy(~Treatment + Type, data=CO2, FUN=fun)
#'
#' # Same as
#' lapply(splitBy(~Treatment + Type, data=CO2), FUN=fun)

#' @export
#' @rdname by-lapply
lapply_by <- function(data, formula, FUN, ...){
    cl   <- match.call(expand.dots = TRUE)
    cl[[2]] <- formula
    cl[[3]] <- data
    names(cl)[2:3] <- c("formula", "data")
    cl[[1]] <- as.name("lapplyBy")
    eval(cl)
}

#' @export
#' @rdname by-lapply
lapplyBy <- function (formula, data = parent.frame(), FUN, ...) 
{
    out <- splitBy(formula, data = data)
    gr  <- unique(attr(out, "grps"))
    ##print(gr)    
    out <- lapply(out, FUN, ...)
    out <- out[gr]
    out
}


#' @export
#' @rdname by-lapply
sapply_by <- function(data, formula, FUN, ..., simplify = TRUE, USE.NAMES = TRUE){
    cl   <- match.call(expand.dots = TRUE)
    cl[[2]] <- formula
    cl[[3]] <- data
    names(cl)[2:3] <- c("formula", "data")
    cl[[1]] <- as.name("sapplyBy")
    eval(cl)
}

#' @export
#' @rdname by-lapply
sapplyBy <- function (formula, data = parent.frame(), FUN, ..., simplify = TRUE, USE.NAMES = TRUE) 
{
    out <- splitBy(formula, data = data)
    gr  <- unique(attr(out, "grps"))
    ##print(gr)    
    #print(out)
    #print(FUN)
    out <- sapply(out, FUN, ..., simplify=simplify, USE.NAMES=USE.NAMES)
    if (!simplify)
        out <- out[gr] 
    out
}



