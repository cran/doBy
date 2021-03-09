###############################################################################
#'
#' @title Function to make groupwise transformations
#' @description Function to make groupwise transformations of data by applying
#'     the transform function to subsets of data.
#' @name by-transform
#' 
###############################################################################
#'
#' @details The ... arguments are tagged vector expressions, which are evaluated
#'     in the data frame data. The tags are matched against names(data), and for
#'     those that match, the value replace the corresponding variable in data,
#'     and the others are appended to data.
#' 
#' @param formula A formula with only a right hand side, see examples below
#' @param data A data frame
#' @param \dots Further arguments of the form tag=value
#' @return The modified value of the dataframe data.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' 
#' @seealso \code{\link{orderBy}}, \code{\link{order_by}}, \code{\link{summaryBy}}, \code{\link{summary_by}},
#'     \code{\link{splitBy}}, \code{\link{split_by}}
#' @keywords univar
#' @examples
#'  
#' data(dietox)
#' transformBy(~Pig, data=dietox, minW=min(Weight), maxW=max(Weight), 
#'     gain=diff(range(Weight)))
#' 

#' @export 
#' @rdname by-transform
transform_by <- function (data, formula, ...) {
    cl <- match.call(expand.dots = TRUE)
    cl[[2]] <- formula
    cl[[3]] <- data
    names(cl)[2:3] <- c("formula", "data")
    cl[[1]] <- as.name("transformBy")
    eval(cl)
}

#' @export
#' @rdname by-transform 
transformBy <- function (formula, data, ...) {

    if (!inherits(data, "tbl_df")) is.tib = FALSE
    else {is.tib = TRUE; data = as.data.frame(data)}
    
    transform2<- function (data, ...) 
    {
        e <- eval(substitute(list(...)), data, parent.frame())
        tags <- names(e)
        inx <- match(tags, names(data))
        matched <- !is.na(inx)
        if (any(matched)) {
            data[inx[matched]] <- e[matched]
            data <- data.frame(data)
        }
        if (!all(matched)) {
            for (i in 1:length(e))
                data[, names(e)[i]] <- e[i]
        }
        return(data)
    }
    
    ddd <- splitBy(formula, data=data, drop=TRUE)
    ee <- lapply(ddd, function(d){
        transform2(d, ...)
    })
    
    out  <- do.call("rbind", ee)
    rownames(out) <- NULL

    if (is.tib) as_tibble(out) else out
    ##out
}

