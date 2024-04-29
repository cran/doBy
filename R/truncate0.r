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
