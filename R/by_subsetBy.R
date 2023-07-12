###############################################################################
#'
#' @title Finds subsets of a dataframe which is split by variables in
#'     a formula.
#' @description A data frame is split by a formula into groups. Then
#'     subsets are found within each group, and the result is
#'     collected into a data frame.
#' @name by-subset
#' 
###############################################################################
#' @param formula A right hand sided formula or a character vector of
#'     variables to split by.
#' @param subset logical expression indicating elements or rows to
#'     keep: missing values are taken as false.
#' @param data A data frame.
#' @param select expression, indicating columns to select from a data
#'     frame.
#' @param drop passed on to \code{[} indexing operator.
#' @param join If FALSE the result is a list of data frames (as
#'     defined by 'formula'); if TRUE one data frame is returned.
#' @param \dots further arguments to be passed to or from other methods.
#' @return A data frame.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{splitBy}}, \code{\link{split_by}}
#' @keywords utilities
#' @examples
#' 
#' data(dietox)
#' subsetBy(~Evit, Weight < mean(Weight), data=dietox)


#' @export
#' @rdname by-subset
subset_by <- function(data, formula, subset, select, drop=FALSE, join=TRUE, ...){
    cl <- match.call(expand.dots = TRUE)
    cl[[2]] <- formula
    cl[[3]] <- cl[[4]] # writing 'subset' fails because evaluation is tried
    cl[[4]] <- data

    names(cl)[2:4] <- c("formula", "subset", "data")
    cl[[1]] <- as.name("subsetBy")
    eval(cl)
}


#' @export
#' @rdname by-subset
subsetBy <- function(formula, subset, data=parent.frame(), select, drop=FALSE, join=TRUE, ...){

    out <- splitBy(formula, data=data)
    subsetMissing <- missing(subset)
    selectMissing <- missing(select)  
    e <- substitute(subset)
    out <- lapply(out, 
                  function(x){
                      if (subsetMissing) 
                          r <- TRUE
                      else {
                          r <- eval(e, x, parent.frame())
                          if (!is.logical(r)) 
                              stop("'subset' must evaluate to logical")
                          r <- r & !is.na(r)
                      }
                      if (selectMissing) 
                          vars <- TRUE
                      else {
                          nl <- as.list(1:ncol(x))
                          names(nl) <- names(x)
                          vars <- eval(substitute(select), nl, parent.frame())
                      }
                      x[r, vars, drop = drop]
                  }
                  )
    if (join) do.call("rbind", out) else out
}
