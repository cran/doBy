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


