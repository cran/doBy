###########################################################################
#'
#' @title Split a data frame
#' @description Split a dataframe according to the levels of variables
#'     in the dataframe. The variables to split by can be given as a
#'     formula or as a character vector.
#' @name by-split
#' 
###########################################################################
#'
#' @param formula A character vector or a right hand sided formula.
#' @param ... A character vector or, right hand sided formula or a
#'     listing of variables.
#' @param data A data frame
#' @param omit Logical Should variables split on be omitted from the
#'     output. Defaults to TRUE.
#' 
#' @param x An object.
#' @param n A single integer.  If positive or zero, size for the
#'     resulting object: number of elements for a vector (including
#'     lists), rows for a matrix or data frame or lines for a
#'     function.  If negative, all but the "n" last/first number of
#'     elements of "x".
#'
#' @return A list of dataframes.
#' 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{orderBy}}, \code{\link{order_by}},
#'     \code{\link{summaryBy}}, \code{\link{summary_by}},
#'     \code{\link{transformBy}}, \code{\link{transform_by}} 
#' @keywords utilities
#' @examples
#'
#' split_by(CO2, ~Treatment+Type)
#' split_by(CO2, Treatment, Type)
#' split_by(CO2, c("Treatment", "Type"))
#' split_by(CO2, Treatment)
#' x <- CO2 |> split_by(~Treatment)
#' head(x, 3)
#' tail(x, 3)
#' 
#' x <- CO2 |> split_by(~Treatment, omit=FALSE)
#' head(x, 3)
#' tail(x, 3)
#'
#' ## The "Old" interface
#' splitBy(~Treatment+Type, CO2)
#' splitBy(~Treatment+Type, data=CO2)
#' splitBy(c("Treatment", "Type"), data=CO2)
#'
NULL

split_by_worker <- function(data., rhs, omit=TRUE){ 

    is.tib <- inherits(data., "tbl_df")
    if (is.tib)
        data. = as.data.frame(data.)
    
    grps <- apply(data.[, rhs, drop=FALSE], 1, paste0, collapse="|")

    ## print(grps)
    out <- split(data., grps)
    
    if (omit){
        rhs.idx <- match(rhs, names(data.))
        out <- lapply(out, function(d){
            d[, -rhs.idx]            
        })
    }
        
    if (is.tib) {
        out <- lapply(out, function(d) {
            as_tibble(d)
        })
    }
        
    groupid <- unique(data.[, rhs, drop=FALSE])
    idxvec <- split(1:nrow(data.), grps)
    
    attr(out, "groupid") <- groupid
    attr(out, "idxvec")  <- idxvec
    attr(out, "grps")    <- grps

    class(out) <- c("splitByData", "list")    
    return(out)
}


#' @export 
#' @rdname by-split
split_by <- function(data, ..., omit=TRUE){
    dots <- match.call(expand.dots = FALSE)$...
    rhs <- dots2char(dots)
    split_by_worker(data, rhs, omit=omit)
}

#' @export 
#' @rdname by-split
splitBy <- function(formula, data, omit=TRUE){
    if (inherits(formula, "formula")){
        rhs       <- all.vars(formula[[2]])
    } else {
        rhs <- formula
    }
    split_by_worker(data, rhs, omit=omit)
}



#' @export
print.splitByData <- function(x, ...){
#  print(attr(x,"groupid"))
    print(cbind(listentry=names(x), attr(x,"groupid")))
    return(invisible(x))
}

#' @importFrom utils head
#' @export
#' @rdname by-split
head.splitByData  <- function(x, n=6L, ...){
    lapply(x, head, n=n, ...)
}

#' @importFrom utils tail
#' @export
#' @rdname by-split
tail.splitByData  <- function(x, n=6L, ...){
    lapply(x, tail, n=n, ...)
}










#' @export 
#' @rdname by-split
#' @param drop Obsolete
split_by.legacy <- function(data, formula, drop=TRUE){
    arg <- list(formula=formula, data=data, drop=drop)
    do.call(splitBy.legacy, arg)
}

#' @export 
#' @rdname by-split
splitBy.legacy <-function (formula, data = parent.frame(), drop=TRUE) {
                                        #, return.matrix=FALSE){

    if (!inherits(data, "tbl_df")) is.tib = FALSE
    else {is.tib = TRUE; data = as.data.frame(data)}
    
    data.var  <- names(data)
    
    if (!(inherits(formula, c("formula", "character"))))        
        stop("'formula' must be a right hand sided formula or a character vector")
    
    if (inherits(formula, "formula")){
        rhs       <- formula[[2]]
        rhs.var   <- all.vars(rhs)
    } else {
        rhs.var <- formula
    }
    
    cls <- lapply(data, class)
    factor.columns <- which(unlist(lapply(cls, function(x) any( x %in% "factor"))))
                                        #print(factor.columns)

    cls       <- lapply(data, class)
    num.idx   <- cls %in% c("numeric","integer")
    fac.var   <- data.var[ !num.idx ]
    
    rhs.fac   <- intersect( rhs.var, data.var )

    if ("." %in% rhs.var){
        ## need all factors not mentioned elsewhere as grouping factors
        rhs.fac <- union( fac.var, rhs.fac)
    }

    
    ## str(list(rhs.var=rhs.var, rhs.fac=rhs.fac, fac.var=fac.var))
    
    rh.trivial <- length( rhs.var ) == 0 
    
    ## FIXME rhs.fac, rhs.var -- clean up!!!
    ## Use: data, rhs.fac, rh.trivial
    if ( rh.trivial ){
        grps <- rep.int(1, nrow(data))
        unique.grps <- 1
        rh.idx    <- 1
    } else {
        grps <- .get_rhs_string( data, rhs.fac, sep.string="|")
        unique.grps <- unique(grps)
        rh.idx    <- match(unique.grps, grps)
    }
    
    out_list <- vector("list", length(unique.grps))

    names(out_list) <- unique.grps

    for (ii in 1:length(unique.grps)){
        dd <- data[unique.grps[ ii ] == grps, ,drop=FALSE]
        if (drop && length(factor.columns)>0){
            for (jj in 1:length(factor.columns)){
                dd[, factor.columns[jj]] <- factor(dd[, factor.columns[jj]])
            }
        }
        if (is.tib) dd <- as_tibble(dd)
        out_list[[ ii ]] <- dd
    }
    
    idxvec <- vector("list", length(unique.grps))
    names(idxvec) <- unique.grps
    for (ii in 1:length(unique.grps)){
        idxvec[[ii]] <- which(grps == unique.grps[ii])
    }
    
    groupid <- data[rh.idx, rhs.fac, drop=FALSE]
    rownames(groupid) <- 1:nrow(groupid)
    
    attr(out_list,"groupid") <- groupid
    attr(out_list,"idxvec")  <- idxvec
    attr(out_list,"grps")    <- grps
    
    class(out_list) <- c("splitByData", "list")    
    out_list
}


