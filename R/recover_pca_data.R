##' Recover data from principal component analysis
##' 
##' Recover data from principal component analysis based on the first (typically few) components.
##' 
##' @param object An object of class `prcomp`.
##' @param comp The number of components to be used. Must be smaller
##'     than the number of variables.
##' 
##' @return A dataframe
##' 
##' @examples
##' 
##' crime <- doBy::crimeRate
##' rownames(crime) <- crime$state
##' crime$state <- NULL
##' 
##' o <- order(apply(scale(crime), 1, sum))
##' dat <- crime[o,]
##' head(dat)
##' tail(dat)
##' matplot(scale(dat), type="l")
##' 
##' pc1 <- prcomp(dat, scale. = TRUE)
##' summary(pc1)
##' rec2 <- recover_pca_data(pc1, 2)
##' 
##' pairs(rec2)
##' 
##' par(mfrow=c(1,2))
##' matplot(scale(dat), type="l")
##' matplot(scale(rec2), type="l")
##' 
##' j <- merge(dat, rec2, by=0)
##' pairs(j[,-1])
##'
##' @export
recover_pca_data <- function (object, comp = 1) 
{
    if (!inherits(object, "prcomp"))
        stop("'object' must be of class 'prcomp'.")
    
    if (length(comp) == 1) 
        comp <- 1:comp
    
    if (is.null(object$x)){
        stop("Can not 'recover' data.\n")
    }

    ## x <- object$x
    rot <- object$rot
    vn <- row.names(rot)
    if (object$scale[1]) {
        rot <- diag(object$scale) %*% rot
    }
    pred <- object$x[, comp, drop = FALSE] %*% t.default(rot[, comp, drop = FALSE])

    if (object$center[1]) {
        pred <- t(t(pred) + object$center)
    }
    colnames(pred) <- vn
    return(pred)
}



