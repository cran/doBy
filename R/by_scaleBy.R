###########################################################################
##' @title Scaling numerical values
##'
##' @description Similar to 'base::scale' but applies to scales / centers only numeric values
##' in data.
##'
##' @name scale_df
###########################################################################
##' 
##' @param x dataframe or matrix
##' @param center Logical, should data be centered.
##' @param scale Logical, should data be scaled.
##'
##' @details
##'
##' * If `x` is not a dataframe, then base::scale is invoked on
##'   `x`.
##'
##' * Suppose `x` is a dataframe. Then base::scale is invoked
##'     on all columns that are numeric, integer or logical.
##' 
##' @return An object of same class as `x`
##' @examples
##' 
##' scale2(iris)

#' @rdname scale_df
#' @export
scale_df <- function(x, center = TRUE, scale = TRUE){

    if (!is(x, "data.frame")){
        return(scale(x, center=center, scale=scale))
    } else { ## x is dataframe
            
        b <- sapply(x,
                    function(z){is(z, c("numeric")) || is(z, c("integer")) || is(z, c("logical")) })
        
        if (!any(b)){ ## x only has numeric values
            return(scale(x, center=center, scale=scale))
        } else { ## x is dataframe with non-numerics
            
            x2 <- x[,b, drop=FALSE]
            x2 <- scale(x2, center=center, scale=scale)
            x[, b] <- x2

            if (!is.null(a <- attributes(x2)$"scaled:center"))
                attr(x, "scaled:center") <- a
            
            if (!is.null(a <- attributes(x2)$"scaled:scale"))
                attr(x, "scaled:scale") <- a
                        
            return(x)
            
        }
            
    }

}

#' @rdname scale_df
#' @export
scale2 <- scale_df



## scale2 <- function(x, center = TRUE, scale = TRUE){

##     if (!inherits(x, c("data.frame", "matrix")))
##         stop("'x' must be matrix or dataframe.\n")
        
##     if (inherits(x, "matrix")){
##         scale(x, center=center, scale=scale)
##     } else {
##         b <- sapply(x, is.numeric)
##         if (!any(b))
##             stop("No numeric value in data; can not scale.\n")
        
##         sc <- scale(x[, b], center=center, scale=scale)        
##         x[,b] <- sc
        
##         if (!is.null(a <- attributes(sc)$"scaled:center"))
##             attr(x, "scaled:center") <- a
        
##         if (!is.null(a <- attributes(sc)$"scaled:scale"))
##             attr(x, "scaled:scale") <- a
        
##         x    
##     }        
## }







###########################################################################
#'
#' @title Scale a dataframe or matrix
#' @description Split a dataframe into a list according to the levels
#'     of variables in the dataframe and scale the numeric variables
#'     in each dataframe in the list.
#' @name by_scale
#' 
###########################################################################
#'
#' @param formula Variables to split data frame by, as `as.quoted`
#'     variables, a formula or character vector.
#' @param data A dataframe or matrix
##' @param center Logical, should data be centered.
##' @param scale Logical, should data be scaled.
#' 
#' @return A list of objects of same class as `x`
#' 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{orderBy}}, \code{\link{order_by}},
#'     \code{\link{summaryBy}}, \code{\link{summary_by}},
#'     \code{\link{transformBy}}, \code{\link{transform_by}} 
#' @keywords utilities
#' 
#' @examples
#'
#' scaleBy(~Species, data=iris, center=TRUE, scale=FALSE)
#' scaleBy(~1, data=iris, center=TRUE, scale=FALSE)
#'
#' scale_by(iris, ~Species)
#' scale_by(iris, ~1)
#' 
#' ## Not combine list of dataframes to one dataframe e.g. as:
#' a <- scale_by(iris, ~Species)
#' d <- do.call(rbind, a)

#' @rdname by_scale
#' @export
scaleBy <- function(formula, data=parent.frame(), center=TRUE, scale=TRUE){
    lapplyBy(formula, data=data, scale2, center=center, scale=scale)        
}

#' @rdname by_scale
#' @export
scale_by <- function(data, formula, center=TRUE, scale=TRUE){
    arg <- list(formula=formula, data=data, center=center, scale=scale)
    do.call(scaleBy, arg)
}   


